;;; init-core.el --- Emacs Core Configuration. -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Copyright (C) 2021  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: Emacs Package Management.


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; For package managment and crucial packages.

;;; Code:

;; Initialize Elpaca Package Manager
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Elpaca lock file for reproducible package versions
(customize-set-variable 'elpaca-lock-file
                        (expand-file-name "elpaca-lock.el" user-emacs-directory))

(defun my/elpaca-write-lock-file ()
  "Write lock file to `elpaca-lock-file'."
  (interactive)
  (if elpaca-lock-file
      (progn (elpaca-write-lock-file elpaca-lock-file)
             (message "Lock file written to %s" elpaca-lock-file))
    (user-error "elpaca-lock-file is not set")))

;; Install use-package support for Elpaca
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; use-package default args
;; (Built-in)
(use-package use-package
  :ensure nil
  :custom
  (use-package-always-ensure        t)
  (use-package-always-defer         nil)
  (use-package-always-demand        t)
  (use-package-expand-minimally     t)
  (use-package-enable-imenu-support t))

;; Emacs Basic Hack
(use-package emacs
  :ensure nil
  :config
  (setq auth-source-save-behavior nil)
  ;; ########## enable some commands
  (progn (defvar enabled-functions '(dired-find-alternate-file
                                     narrow-to-region
                                     upcase-region
                                     downcase-region))
         (dolist (want-enabled-function enabled-functions)
           (put want-enabled-function 'disabled nil))))

;; no-littering (GitHub)
;; Help for keeping Emacs Configuration Dir clean.
;; Built-in packages `recentf` needed
(use-package no-littering
  :ensure (:host github :repo "emacscollective/no-littering" :wait t)
  :demand t)

(use-package persistent-cached-load-filter
  :ensure (:host github :repo "include-yy/persistent-cached-load-filter")
  :config
  (persistent-cached-load-filter-easy-setup))


(provide 'init-core)

;;; init-core.el ends here
