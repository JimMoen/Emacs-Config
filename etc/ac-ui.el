;;; ac-ui.el --- UI, Color, Theme. -*- lexical-binding: t -*-

;; Copyright (C) 2021  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: UI Theme


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

;; UI, Color, Theme.

;;; Code:

;; Emacs Built-in Features
(use-package emacs
  :ensure nil
  :config
  (progn                                                  ;; Disable GUI Features
    (setq use-file-dialog                   nil
          use-dialog-box                    nil
          inhibit-startup-screen            t
          inhibit-startup-echo-area-message t)
    (tool-bar-mode                         -1)
    (menu-bar-mode                         -1)
    (scroll-bar-mode                       -1))
  (setq scroll-step 1                                     ;; Friendly and smoothly scroll
        scroll-margin 5
        hscroll-step 1
        hscroll-margin 5
        scroll-conservatively 101
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01
        scroll-preserve-screen-position 'always)
  (setq auto-window-vscroll            nil)               ;; Disable auto height of line
  (setq truncate-partial-width-windows t)                 ;; Disable line truncate
  (setq inhibit-compacting-font-caches t)                 ;; Don’t compact font caches during GC.
  (defalias 'yes-or-no-p 'y-or-n-p)                       ;; Use y-or-n instead of yes-or-no
  (progn (setq JimMoen/frame-settings '((font . "等距更纱黑体 SC-12")
                                        (alpha . (88 83))))
         (dolist (frame-set-var JimMoen/frame-settings)
           (push frame-set-var default-frame-alist))))

;; Doom Modeline (Melpa)
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon nil)            ;; Do not use icon
  :defer t
  :config
  (setq
   doom-modeline-mode-height               20
   doom-modeline-bar-width                 3
   doom-modeline-window-width-limit        fill-column
   doom-modeline-buffer-file-name-style    'truncate-nil
   doom-modeline-github                    t
   doom-modeline-github-interval           (* 30 60)
   doom-modeline-project-detection         'projectile
   all-the-icons-color-icons               nil))

;; color-theme-sanityinc-tomorrow (Melpa)
(use-package color-theme-sanityinc-tomorrow
  :init
  (setq custom-safe-themes t)
  (load-theme 'sanityinc-tomorrow-night))


(provide 'ac-ui)

;;; ac-ui.el ends here
