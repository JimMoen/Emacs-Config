;;; init.el --- Emacs Core Configuration. -*- mode: emacs-lisp; lexical-binding: t; -*-

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
;; (setq package-archives '(("gnu" . "https://elpa.zilongshanren.com/gnu/")
;;                          ("melpa" . "https://elpa.zilongshanren.com/melpa/")
;;                          ("org" . "https://elpa.zilongshanren.com/org/")))
(setq package-archives
      '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
        ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
        ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))


;; Initialize packages
(unless (bound-and-true-p package--initialized)             ;; Avoid warnings in 27
  (setq package-check-signature nil)                        ;; Check signature when installing
  (package-initialize))

;; Setup 'use-package'
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

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
      no-littering restart-emacs disable-mouse

      ;; ########## base packages
      ;; [Built-in] dired autorevert recentf bookmark ibuffer winner
      ;; [Site]     None
      general ivy counsel swiper which-key
      dired-single switch-window shackle

      ;; ########## ui packages
      ;; [Built-in] None
      ;; [Site]     None
      dashboard dashboard-ls
      doom-modeline kaolin-themes

      ;; ########## base editing
      ;; [Built-in] align display-line-numbers delsel
      ;;            so-long subword whitespace hideshow
      ;; [Site]     None
      avy rainbow-mode smartparens
      rainbow-delimiters
      wgrep multiple-cursors

      ;; ########## other utils
      ;; [Built-in] calendar
      ;; [Site]     english-teacher
      rime youdao-dictionary use-proxy
      keychain-environment
      cal-china-x
      speed-type pdf-tools info-colors

      ;; ########## Development Tools
      ;; [Built-in] None
      ;; [Site]     None
      magit projectile counsel-projectile persp-mode
      company company-tabnine prescient ivy-prescient company-prescient
      flycheck yasnippet yasnippet-snippets
      lsp-mode lsp-ui lsp-ivy))

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
;; Built-in packags "recentf" needed
(use-package no-littering
  :demand t)

;; restart-emacs (Melpa)
;; Easy to Restart Emacs
(use-package restart-emacs
  :demand t)

;; disable-mouse (Melpa)
;; Disable the mouse action
(use-package disable-mouse
  :demand t
  :hook
  (after-init . global-disable-mouse-mode))


(provide 'aa-core)

;;; aa-core.el ends here
