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
;;   |       |----- init-core.el          ;; For package managent and crucial packages.
;;   |       |----- init-base.el          ;; For better Emacs Framework all the time.
;;   |       |----- init-ui.el            ;; Built-in UI.  Modeline, Color Theme.
;;   |       |----- init-editing.el       ;; Better Editing.
;;   |       |----- init-utils.el         ;; Utility packages.
;;   |       |----- init-dev-tools.el     ;; Development Tools.
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
(require 'init-core)
;; For package managent and crucial packages.
;; Set Package-Archive. Initialize use-package.
;; Hack Built-in 'package-selected-packages'
;; Enable some disabled features.

(require 'init-base)
;; For better Emacs Framework all the time.

(require 'init-ui)
;; For More Beautiful Emacs.

(require 'init-editing)
;; For Better editing

(require 'init-utils)
;; Some Useful packages.

(require 'init-dev-tools)
;; Development Tools.
;; ########## PACKAGES

(require-all-elisp-in-directory "etc/init-dev-lang")
;; For Programming languages


(when (file-exists-p custom-file)
  (load custom-file))


(provide 'init)

;;; init.el ends here
