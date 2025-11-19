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


(provide 'init-lib)

;;; init-lib.el ends here
