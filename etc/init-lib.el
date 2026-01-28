;;; init-lib.el --- Custom Lisp Library -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains custom utility functions used throughout the Emacs configuration.

;;; Code:

;; ########## Require file(s) in directory func.
(defun require-all-elisp-in-directory (directory-name)
  "Require all '.el' files in DIRECTORY-NAME.
Off course, it will add DIRECTORY-NAME to `load-path' automaticly.

DIRECTORY-NAME must be a relative path like \"etc/dir\".
It will be expanded within `user-emacs-directory'."
  (let ((elisp-directory-true-path (expand-file-name directory-name user-emacs-directory)))
    (push elisp-directory-true-path load-path)
    (mapc (lambda (name)
            (require (intern (file-name-sans-extension name))))
          (directory-files elisp-directory-true-path  nil "\\.el$"))))


;; ########## Get current file path func.
(defun my/copy-file-path ()
  "Copy the current buffer's file path to kill-ring.
Paths under home directory are abbreviated with ~."
  (interactive)
  (if-let ((file-path (buffer-file-name)))
      (let ((abbreviated-path (abbreviate-file-name file-path)))
        (kill-new abbreviated-path)
        (message "Copied: %s" abbreviated-path))
    (message "Buffer is not visiting a file")))

(provide 'init-lib)

;;; init-lib.el ends here
