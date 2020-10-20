;;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;;; File: init.el
;;;; Auth: JimMoen
;;;; Code:


;;;;  ==== [I] means git-ignore    ====
;;;;  ==== [S] means git-submodule ====
;;;;
;;;;  ~/.config/emacs/
;;;;   |----- early-init.el                 ;; Coding System. Garbage Collection. Startup timer.
;;;;   |----- init.el                       ;; Some Core Settings required by other elisp file.
;;;;   |----- etc/                          ;; Elisp files. File loading order is determined by the letter before the file name.
;;;;   |       |----- aa-core.el            ;; For package managent and crucial packages.
;;;;   |       |----- ab-base.el            ;; For better Emacs Framework all the time.
;;;;   |       |----- ac-ui.el              ;; Built-in UI. Modeline, Color Theme.
;;;;   |       |----- ad-editing.el         ;; Better Editing.
;;;;   |       |----- ae-utils.el           ;; Some Useful packages.
;;;;   |       |----- ba-dev-tools.el       ;; Developeing Core Tools.
;;;;   |       |----- bb-dev-complete.el    ;; For Program Language completing.
;;;;   |       |----- zz-testing.el         ;; For testing packages.
;;;;   |       `--[I] custom.el             ;; Custom file.
;;;;   |--[I] var/
;;;;   |       |---@@ `no-littering.el      ;; Store packages using files.
;;;;   |       `----- [DIRS & FILES]        ;; (i.e) 'recentf', 'bookmark' 'projectile'...
;;;;   |--[I] elpa/
;;;;   |       |---@@ `package.el           ;; Download from elpa and melpa etc...
;;;;   |       `----- [DIRS]                ;; Packages directories.
;;;;   `--[S] site-lisp/
;;;;           |---@@ `GIT-SUBMODULE        ;; Lisp packages not managed by package.el (directories). Management by git-submodule.
;;;;           `----- [DIRS]                ;; Site Packages directories.


;;;; ####################################################### Load Path
(defvar my-lisp-dirs '("etc"))
(setq custom-file (expand-file-name "./etc/custom.el" user-emacs-directory))

(defun update-load-path ()
  "Update `load-path'."
  (let* ()
    (dolist (each-dir my-lisp-dirs)
      (push (expand-file-name each-dir user-emacs-directory) load-path))))

(update-load-path)

;;;; ####################################################### Open init file

(defun open-my-init-dir()
  "To open my init dir faster."
  (interactive)
  (let ((default-directory "~/.config/emacs"))
    (projectile-dired)))
(global-set-key (kbd "<f2>") 'open-my-init-dir)
;; End of open init.el


(require 'aa-core)
;;;; #######################################################
;;;; For package managent and crucial packages.
;;;; Set Package-Archive. Initialize use-package.
;;;; Hack Built-in 'package-selected-packages'
;;;; Enable some disabled features.
;;;; #################### PACKAGES
;;;; use-package-ensure-system-package
;;;; no-littering restart-emacs disable-mouse


(require 'ab-base)
;;;; #######################################################
;;;; For better Emacs Framework all the time.
;;;; #################### PACKAGES
;;;; ivy counsel swiper
;;;; dired dired-single autorevert recentf bookmark
;;;; PERSONAL-KEYBINDINGS which-key
;;;; Ibuffer switch-window shackle winner(Built-in)


(require 'ac-ui)
;;;; #######################################################
;;;; For More Beautiful Emacs.
;;;; #################### PACKAGES
;;;; doom-modeline
;;;; color-theme-sanityinc-tomorrow


(require 'ad-editing)
;;;; #######################################################
;;;; For Better editing
;;;; #################### PACKAGES
;;;; INDENT-SETTINGS align
;;;; delsel hl-line whitespace avy
;;;; hideshow rainbow-mode
;;;; parn smartparens rainbow-delimiters


(require 'ae-utils)
;;;; #######################################################
;;;; Some Useful packages.
;;;; #################### PACKAGES
;;;; rime
;;;; Youdao english-teacher(None melpa)
;;;; speed-type


(require 'ba-dev-tools)
;;;; #######################################################
;;;; Developeing Core Tools.
;;;; #################### PACKAGES
;;;; magit projectile counsel-projectile
;;;; persp-mode-projectile-bridge(None melpa)
;;;; flycheck


(require 'bb-dev-complete)
;;;; #######################################################
;;;; For Program Language completing.
;;;; #################### PACKAGES
;;;; company
;;;; lsp lsp-ui lsp-ivy


(require 'zz-testing)
;;;; #######################################################
;;;; For testing packages.
;;;; #################### PACKAGES
;;;;



(load custom-file)

;;;; #######################################################
;;;; init.el ends here
