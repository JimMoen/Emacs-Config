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
In `dired' and `magit' buffers, copy the directory path instead.
Paths under home directory are abbreviated with ~."
  (interactive)
  (if-let ((file-path (or (buffer-file-name)
                          (and (derived-mode-p 'dired-mode)
                               default-directory)
                          (and (derived-mode-p 'magit-mode)
                               (magit-toplevel)))))
      (let ((abbreviated-path (abbreviate-file-name file-path)))
        (kill-new abbreviated-path)
        (message "Copied: %s" abbreviated-path))
    (message "Buffer is not visiting a file")))

;; ########## Package Registry Export
(defcustom my/package-registry-max-backups 5
  "Maximum number of backup files to keep for package registry."
  :type 'integer
  :group 'convenience)

(defun my/export-package-registry ()
  "Export all Elpaca declared packages grouped by config file.
Output to `var/package-registry.txt' with rotation of old backups."
  (interactive)
  (require 'elpaca nil t)
  (let* ((var-dir (expand-file-name "var/" user-emacs-directory))
         (registry-file (expand-file-name "package-registry.txt" var-dir))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (etc-dir (expand-file-name "etc/" user-emacs-directory))
         (config-files (directory-files-recursively etc-dir "\\.el$"))
         (config-files (cons (expand-file-name "init.el" user-emacs-directory) config-files))
         (package-file-map (make-hash-table :test 'equal))
         (file-order '()))
    ;; Ensure var/ exists
    (unless (file-directory-p var-dir)
      (make-directory var-dir t))
    ;; Rotate existing file
    (when (file-exists-p registry-file)
      (let* ((backup-name (expand-file-name
                           (format "package-registry-%s.txt" timestamp) var-dir))
             (backups (sort (file-expand-wildcards
                             (expand-file-name "package-registry-*.txt" var-dir))
                            #'string<)))
        (rename-file registry-file backup-name)
        ;; Remove oldest backups beyond max
        (when (> (length backups) (1- my/package-registry-max-backups))
          (dolist (old (butlast backups (1- my/package-registry-max-backups)))
            (delete-file old)))))
    ;; Build package -> config file mapping
    (dolist (file config-files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "(use-package[[:space:]]+\\([^[:space:]\n)]+\\)" nil t)
          (let* ((pkg (match-string 1))
                 (rel-file (file-relative-name file user-emacs-directory)))
            (unless (gethash rel-file package-file-map)
              (puthash rel-file '() package-file-map)
              (push rel-file file-order))
            (puthash rel-file
                     (append (gethash rel-file package-file-map) (list pkg))
                     package-file-map)))))
    ;; Write registry
    (with-temp-file registry-file
      (insert (format ";;; Elpaca Package Registry - Generated %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format ";;; Total config files: %d\n\n" (length file-order)))
      (dolist (file (nreverse file-order))
        (let ((pkgs (gethash file package-file-map)))
          (insert (format "## %s (%d packages)\n" file (length pkgs)))
          (dolist (pkg pkgs)
            (insert (format "  - %s\n" pkg)))
          (insert "\n"))))
    (message "Package registry exported to %s" registry-file)))

(add-hook 'elpaca-after-init-hook #'my/export-package-registry)

(provide 'init-lib)

;;; init-lib.el ends here
