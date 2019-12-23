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

(defun kube-run (&rest args)
  "Execute a kubectl command with ARGS."
  (let ((command (concat kube-kubectl-path " " (string-join args " "))))
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

(defun kube-logs (name follow)
  "View logs for all containers in a pod."
  ;; TODO: Allow for specifying container.
  (interactive (list (kube-read-pod) (y-or-n-p "Follow? ")))
  (let ((buf (pop-to-buffer (format "*kube logs %s*" name) nil)))
    (async-shell-command
     (concat kube-kubectl-path " logs " name (when follow " -f ") " --all-containers=true")
     buf)))

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
           (alist-get 'startTime status)))))

(defun kube--table-refresh ()
  "Refresh table."
  (let ((pods (kube--get-pods)))
    (setq tabulated-list-entries (mapcar #'kube--table-entry pods))))

(define-infix-argument kube-logs-follow ()
  :description "Follow logs"
  :class 'transient-switch
  :argument "-f")

(define-suffix-command kube-logs-run (args)
  (interactive (list (transient-args current-transient-command)))
  (kube-logs (tabulated-list-get-id) (member "-f" args)))

(define-transient-command kube-log-popup ()
  "Gets logs for a pod."
  ["Arguments"
   ("-f" kube-logs-follow)]
  ["Actions"
   ("L" "Logs" kube-logs-run)])

(define-infix-argument kube-delete-force ()
  :description "Force pod deletion"
  :class 'transient-switch
  :argument "--force")

(define-suffix-command kube-delete-run (args)
  (interactive (list (transient-args current-transient-command)))
  (kube-delete (tabulated-list-get-id) (member "--force" args)))

(define-transient-command kube-delete-popup ()
  "Delete a pod."
  ["Arguments"
   ("--force" kube-delete-force)]
  ["Actions"
   ("D" "Delete" kube-delete-run)])

(define-suffix-command kube-shell-run ()
  (interactive)
  (kube-shell (tabulated-list-get-id)))

(define-suffix-command kube-eshell-run ()
  (interactive)
  (kube-eshell (tabulated-list-get-id)))

(define-transient-command kube-shell-popup ()
  "Open a shell."
  ["Actions"
   ("e" "Eshell" kube-eshell-run)
   ("s" "Shell" kube-shell-run)])

(defvar kube-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "L") 'kube-log-popup)
    (define-key map (kbd "D") 'kube-delete-popup)
    (define-key map (kbd "B") 'kube-shell-popup)
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
      (kbd "L") 'kube-log-popup
      (kbd "D") 'kube-delete-popup
      (kbd "B") 'kube-shell-popup))

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
