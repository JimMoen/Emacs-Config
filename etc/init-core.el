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

;; Initialize Package Management System
;; Package Archives
(setq package-archives
      '(;; only for package `org-plus-contrib`
        ;; not required now
        ;; ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; Initialize packages
(unless (bound-and-true-p package--initialized)             ;; Avoid warnings in 27
  (setq package-check-signature nil)                        ;; Check signature when installing
  (package-initialize))

;; Setup 'straight.el' and 'use-package'
;; (unless (package-installed-p 'use-package)
;;   (unless package-archive-contents
;;     (package-refresh-contents))
;;   (package-install 'use-package))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)
  (with-no-warnings
    (with-eval-after-load 'straight
      (straight-use-package 'use-package))))

;; use-package default args
(setq use-package-always-ensure        t                    ;; Always ensure
      use-package-always-defer         nil                  ;; Never defer
      use-package-always-demand        t                    ;; Always demand
      use-package-expand-minimally     t                    ;; Be Silent
      use-package-enable-imenu-support t)                   ;; Enable imenu for use-package


;; Emacs Basic Hack
(use-package emacs
  :ensure nil
  :config
  (setq auth-source-save-behavior nil)
  (defvar my-packages
    '(;; ########## core packages
      ;; [Built-in] None
      ;; [Site]     None
      use-package use-package-ensure-system-package
      no-littering disable-mouse

      ;; ########## base packages
      ;; [Built-in] dired autorevert recentf bookmark ibuffer winner
      ;; [Site]     None
      nerd-icons nerd-icons-ibuffer nerd-icons-dired
      general ivy ivy-rich counsel swiper ivy-hydra ivy-avy counsel-tramp nerd-icons-ivy-rich
      sudo-edit helpful which-key ivy-dired-history dired-single
      ace-window shackle
      edit-server

      ;; ########## ui packages
      ;; [Built-in] None
      ;; [Site]     None
      dashboard dashboard-ls
      doom-modeline doom-themes

      ;; ########## base editing
      ;; [Built-in] align display-line-numbers delsel
      ;;            so-long subword whitespace hideshow
      ;; [Site]     None
      avy rainbow-mode smartparens
      vundo ligature
      rainbow-delimiters
      wgrep multiple-cursors
      editorconfig
      flycheck-aspell

      ;; ########## other utils
      ;; [Built-in] calendar
      ;; [Site]     english-teacher
      youdao-dictionary use-proxy
      ssh-config-mode cal-china-x
      i3wm-config-mode systemd pkgbuild-mode
      speed-type pdf-tools info-colors
      restclient company-restclient
      protobuf-mode
      pos-tip ;; required by youdao-dictionary
      direnv

      ;; ########## Development Tools
      ;; [Built-in] None
      ;; [Site]     None
      magit magit-delta diff-hl git-timemachine
      projectile counsel-projectile persp-mode
      treemacs treemacs-projectile treemacs-nerd-icons treemacs-magit treemacs-persp
      company company-box company-tabnine prescient ivy-prescient company-prescient
      flycheck yasnippet yasnippet-snippets
      lsp-mode lsp-ui lsp-ivy lsp-treemacs
      tree-sitter tree-sitter-langs
      apheleia

      ;; ########## Programming Language Support
      ;; [Built-in] python
      ;; [Site]     None
      yaml-mode yaml-imenu
      erlang lsp-pyright elixir-mode
      haskell-mode
      rust-mode cargo-mode
      go-mode python-mode
      php-mode js2-mode dart-mode jq-mode
      protobuf-ts-mode ;; proto3 only
      hcl-mode ;; for .hocon files
      typescript-mode ;; Vue && typescript && JavaScript
      ))

  ;; ########## enable some commands
  (progn (defvar enabled-functions '(dired-find-alternate-file
                                     narrow-to-region
                                     upcase-region
                                     downcase-region))
         (dolist (want-enabled-function enabled-functions)
           (put want-enabled-function 'disabled nil)))
  ;; ########## avoid package.el "custom-set-variable" in custom.el
  (progn (defun my-save-selected-packages (&optional package-list)
           "set and (don't!) save `package-selected-packages' to value."
           (when package-list
             (setq package-selected-packages package-list))
           (unless after-init-time
             (add-hook 'after-init-hook #'package--save-selected-packages)))
         (advice-add 'package--save-selected-packages :override #'my-save-selected-packages)
         (my-save-selected-packages my-packages)))


;; Initialize Crucial Package
;; use-package-ensure-system-package (Melpa)
;; For system packages. Such as "ripgrep"
(use-package use-package-ensure-system-package
  :demand t)

;; no-littering (Melpa)
;; Help for keeping Emacs Configuration Dir clean.
;; Built-in packages `recentf` needed
(use-package no-littering
  :demand t)

;; disable-mouse (Melpa)
;; Disable the mouse action
(use-package disable-mouse
  :demand t
  :hook
  (after-init . global-disable-mouse-mode))


(provide 'init-core)

;;; init-core.el ends here
