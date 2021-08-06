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
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

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
  (setq my-packages
        '(;; ########## core packages
          ;; [Built-in] None
          ;; [Site]     None
          use-package use-package-ensure-system-package
          no-littering restart-emacs disable-mouse

          ;; ########## base packages
          ;; [Built-in] dired autorevert recentf bookmark ibuffer winner
          ;; [Site]     None
          ivy counsel swiper which-key
          dired-single persp-mode switch-window shackle))

  ;; ########## enable some commands
  (progn (setq enabled-functions '(dired-find-alternate-file
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
