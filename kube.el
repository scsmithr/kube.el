;;; kube.el --- kubectl emacs wrapper -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Sean Smith <scsmithr@gmail.com>

;; This file is part of kube.el.

;; kube.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; gcloud.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with gcloud.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'tablist)
(require 'parse-time)
(require 'transient)

(eval-when-compile
  (require 'subr-x))

(defvar kube-kubectl-path "kubectl"
  "Path to kubectl binary.")

(defvar kube-tramp-method "kube"
  "Tramp method name.")

;;;###autoload
(defun kube-tramp-add-method ()
  "Add kube to list of tramp methods."
  (add-to-list 'tramp-methods
               `(,kube-tramp-method
                 (tramp-login-program ,kube-kubectl-path)
                 (tramp-login-args (("exec" "-it") ("%h") ("sh")))
                 (tramp-remote-shell "sh")
                 (tramp-remote-shell-args ("-i" "-c")))))

;;;###autoload
(eval-after-load 'tramp
  '(progn (kube-tramp-add-method)))

(defun kube-generate-command (&rest args)
  "Generate a kubectl command string using ARGS."
  (concat kube-kubectl-path " " (string-join args " ")))

(defun kube-run (&rest args)
  "Execute a kubectl command with ARGS."
  (let ((command (apply 'kube-generate-command args)))
    (message command)
    (shell-command-to-string command)))

(defun kube-read-pod ()
  "Prompt for a pod name."
  (completing-read "Pod: " (mapcar
                            (lambda (pod)
                              (let ((metadata (alist-get 'metadata pod)))
                                (alist-get 'name metadata)))
                            (kube--get-pods))))

(defun kube-format-tramp (name &optional path)
  (format "/%s:%s:%s" kube-tramp-method name (or path "/")))

(defun kube-dired (name)
  "Open dired inside pod."
  (interactive (list (kube-read-pod)))
  (dired (kube-format-tramp name)))

(defun kube-shell (name)
  "Open shell inside pod."
  (interactive (list (kube-read-pod)))
  (let ((default-directory (kube-format-tramp name)))
    (shell (generate-new-buffer-name (format "*shell %s*" default-directory)))))

(defun kube-eshell (name)
  "Open eshell inside pods."
  (interactive (list (kube-read-pod)))
  (let* ((default-directory (kube-format-tramp name))
         (eshell-buffer-name (generate-new-buffer-name
                              (format "*eshell %s*" default-directory))))
    (eshell t)))

(defun kube-delete (name force)
  "Delete a pod."
  (interactive (list (kube-read-pod) (y-or-n-p "Force? ")))
  (let ((args (append `("delete" "pod" ,name) (when force '("--force"))))
        (buf-name (generate-new-buffer-name (format "*kube delete %s*" name))))
    (apply
     'start-process
     "kube-delete"
     buf-name
     kube-kubectl-path args))
  (kube--table-refresh))

(defun kube ()
  "List kube pods."
  (interactive)
  (pop-to-buffer "*kube*" nil)
  (kube-mode)
  (kube--table-refresh)
  (tablist-revert))

(defun kube--table-entry (pod)
  "Create a table entry from a POD assoc."
  (let* ((metadata (alist-get 'metadata pod))
         (status (alist-get 'status pod))
         (containers (alist-get 'containerStatuses status)))
    (list (alist-get 'name metadata)
          (vector
           (alist-get 'name metadata)
           (format "%d/%d"
                   (seq-reduce #'+
                               (mapcar
                                (lambda (container)
                                  (if (alist-get 'ready container) 1 0))
                                containers)
                               0)
                   (length containers))
           (alist-get 'phase status) ;; status
           (format "%d" (seq-reduce #'+
                                    (mapcar
                                     (lambda (container)
                                       (alist-get 'restartCount container))
                                     containers)
                                    0))
           (kube--pod-age (alist-get 'startTime status))))))

(defun kube--pod-age (start-time)
  (let ((diff (time-to-seconds
               (time-subtract (current-time)
                              (parse-iso8601-time-string start-time)))))
    (car (split-string (format-seconds "%yy,%dd,%hh,%mm,%ss%z" diff) ","))))

(defun kube--table-refresh ()
  "Refresh table."
  (let ((pods (kube--get-pods)))
    (setq tabulated-list-entries (mapcar #'kube--table-entry pods))))

(defun kube-get-transient-action ()
  "Transforms a transient command into args for kubectl."
  (s-replace "-" " " (s-chop-prefix "kube-command-" (symbol-name current-transient-command))))

(defun kube-get-marked ()
  "Return the names of marked pods."
  (mapcar 'car (tablist-get-marked-items)))

(defun kube-generic-action-with-buffer (action args)
  "Perform ACTION async with ARGS, outputting to a buffer.

This will only run with the currently selected pod.

TODO: Switch buffer major mode depending on the ouput format."
  (interactive (list (kube-get-transient-action)
                     (transient-args current-transient-command)))
  (let* ((name (tabulated-list-get-id))
         (buf (pop-to-buffer (format "*kube %s %s*" action name) nil)))
    (async-shell-command
     (kube-generate-command action (string-join args " ") name)
     buf)))

(defun kube-generic-action (action args)
  "Perform ACTION async with ARGS.

All commands will be run async on the selected pods.  The command
will be in the form of `kubectl <action> <args> <pod name(s)>'.
ACTION may be a string containing multiple words, eg `delete
pod'."
  (interactive (list (kube-get-transient-action)
                     (transient-args current-transient-command)))
  (let* ((names (kube-get-marked))
         (buf-name (generate-new-buffer-name (format "*kube %s %s*" action names)))
         (args (append (split-string action) args names)))
    (set-process-sentinel
     (apply
      'start-process
      "kube-async-action"
      buf-name
      kube-kubectl-path args)
     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (kill-buffer buf-name))))))

(define-transient-command kube-command-logs ()
  "Gets logs for a pod."
  :value '("--all-containers=true")
  ["Arguments"
   ("-f" "Follow" "-f")
   ("-a" "All" "--all-containers=true")]
  ["Actions"
   ("L" "Logs" kube-generic-action-with-buffer)])

(define-transient-command kube-command-delete-pod ()
  "Delete a pod."
  ["Arguments"
   ("--force" "Force delete" "--force")]
  ["Actions"
   ("D" "Delete" kube-generic-action)])

(define-transient-command kube-command-get-pod ()
  "Get info for a pod."
  :value '("-o json")
  ["Arguments"
   ("-o" "Output format" "-o " read-string)]
  ["Actions"
   ("G" "Get" kube-generic-action-with-buffer)])

(define-suffix-command kube-shell-selection ()
  (interactive)
  (kube-shell (tabulated-list-get-id)))

(define-suffix-command kube-eshell-selection ()
  (interactive)
  (kube-eshell (tabulated-list-get-id)))

(define-transient-command kube-command-shell ()
  "Open a shell."
  ["Actions"
   ("e" "Eshell" kube-eshell-selection)
   ("s" "Shell" kube-shell-selection)])

(defvar kube-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "L") 'kube-command-logs)
    (define-key map (kbd "D") 'kube-command-delete-pod)
    (define-key map (kbd "B") 'kube-command-shell)
    (define-key map (kbd "G") 'kube-command-get-pod)
    map)
  "Keymap for kube-mode.")

(defun kube--get-pods ()
  "Get an assocation list of pods."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (output (kube-run "get" "pods" "-o" "json"))
         (json (json-read-from-string output)))
    (alist-get 'items json)))

(define-derived-mode kube-mode tabulated-list-mode "kube"
  "Kube derived mode"
  (setq mode-name "kube"
        major-mode 'kube-mode)
  (use-local-map kube-mode-map)

  (eval-after-load 'evil-mode
    (evil-add-hjkl-bindings kube-mode-map 'normal
      (kbd "L") 'kube-command-logs
      (kbd "D") 'kube-command-delete-pod
      (kbd "B") 'kube-command-shell
      (kbd "G") 'kube-command-get-pod))

  (setq tabulated-list-format
        [("Name" 40 t)
         ("Ready" 10 t)
         ("Status" 10 t)
         ("Restarts" 10 t)
         ("Age" 10 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Name" nil))

  (add-hook 'tabulated-list-revert-hook 'kube--table-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'kube)
;;; kube.el ends here
