;;; init.el --- Emacs Init File. -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Copyright (C) 2021  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: init.el


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

;; JimMoen's Personal Emacs Configuration init.el .
;;  ==== [I] means git-ignore    ====
;;  ==== [S] means git-submodule ====
;;
;;  ~/.config/emacs/
;;   |----- early-init.el                 ;; Coding System.  Garbage Collection.  Startup timer.
;;   |----- init.el                       ;; Some Core Settings required by other elisp file.
;;   |----- etc/                          ;; Elisp files.  File loading order is determined by the letter before the file name.
;;   |       |----- aa-core.el            ;; For package managent and crucial packages.
;;   |       |----- ab-base.el            ;; For better Emacs Framework all the time.
;;   |       |----- ac-ui.el              ;; Built-in UI.  Modeline, Color Theme.
;;   |       |----- ad-editing.el         ;; Better Editing.
;;   |       |----- ae-utils.el           ;; Utility packages.
;;   |       |----- ba-dev-tools.el       ;; Development Tools.
;;   |       `--[I] custom.el             ;; Custom file.
;;   |--[I] var/
;;   |       |---@@ `no-littering.el      ;; Store packages using files.
;;   |       `----- [DIRS & FILES]        ;; (i.e) 'recentf', 'bookmark' 'projectile'...
;;   |--[I] elpa/
;;   |       |---@@ `package.el           ;; Download from elpa and melpa etc...
;;   |       `----- [DIRS]                ;; Packages directories.
;;   `--[S] site-lisp/
;;           |---@@ `GIT-SUBMODULE        ;; Lisp packages not managed by package.el (directories).  Management by git-submodule.
;;           `----- [DIRS]                ;; Site Packages directories.

;;; Code:

;; ########## Load Path
;; Add etc/ to load-path
(push (expand-file-name "etc/" user-emacs-directory)
      load-path)
(setq custom-file
      (expand-file-name "./etc/custom.el" user-emacs-directory))

;; ########## Require file(s) in directory func.
(defun require-all-elisp-in-directory (directory-name)
  "Require all '.el' files in DIRECTORY-NAME.
Off course, it will add DIRECTORY-NAME to `load-path' automaticly.\n
DIRECTORY-NAME must be a relative path like \"etc/dir\".
It will be expanded within `user-emacs-directory'."
  (let ((elisp-directory-true-path (expand-file-name directory-name user-emacs-directory)))
    (push elisp-directory-true-path load-path)
    (mapc (lambda (name)
            (require (intern (file-name-sans-extension name))))
          (directory-files elisp-directory-true-path  nil "\\.el$"))))

;; ########## Open init file
(defun open-my-init-dir()
  "To open my init dir faster."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (projectile-dired)))
(global-set-key (kbd "<f2>") 'open-my-init-dir)


;; ########## require files.
(require 'aa-core)
;; For package managent and crucial packages.
;; Set Package-Archive. Initialize use-package.
;; Hack Built-in 'package-selected-packages'
;; Enable some disabled features.

;; ########## PACKAGES
;; use-package-ensure-system-package
;; no-littering restart-emacs disable-mouse


(require 'ab-base)
;; For better Emacs Framework all the time.

;; ########## PACKAGES
;; general
;; ivy counsel swiper
;; dired dired-single autorevert recentf bookmark
;; PERSONAL-KEYBINDINGS which-key
;; Ibuffer switch-window shackle winner(Built-in)


(require 'ac-ui)
;; For More Beautiful Emacs.

;; ########## PACKAGES
;; dashboard dashboard-ls
;; doom-modeline
;; kaolin-themes


(require 'ad-editing)
;; For Better editing

;; ########## PACKAGES
;; EMACS-BUILT-IN-INDENT-SETTINGS align
;; avy
;; hideshow rainbow-mode
;; smartparens rainbow-delimiters
;; wgrep multiple-cursors


(require 'ae-utils)
;; Some Useful packages.
;; ########## PACKAGES
;; rime youdao-dictionary english-teacher(Site)
;; keychain-environment
;; cal-china-x
;; speed-type pdf-tools info-colors


(require 'ba-dev-tools)
;; Development Tools.
;; ########## PACKAGES
;; magit projectile counsel-projectile persp-mode
;; company company-tabnine prescient ivy-prescient company-prescient
;; flycheck yasnippet yasnippet-snippets
;; lsp-mode lsp-ui lsp-ivy


(when (file-exists-p custom-file)
  (load custom-file))


(provide 'init)

;;; init.el ends here
