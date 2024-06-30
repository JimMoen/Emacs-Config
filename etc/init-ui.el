;;; init-ui.el --- UI, Color, Theme. -*- lexical-binding: t -*-

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
  ;; Disable GUI Features
  (tool-bar-mode   -1)
  (menu-bar-mode   -1)
  (scroll-bar-mode -1)
  (setq use-file-dialog                   nil
        use-dialog-box                    nil)
  (setopt ring-bell-function              'ignore
          visible-bell                    nil)
  ;; Scroll
  (setq scroll-step                       0                 ;; Friendly and smoothly scroll
        scroll-margin                     1
        scroll-conservatively             500
        scroll-up-aggressively            0.1
        scroll-down-aggressively          0.1
        hscroll-step                      1
        hscroll-margin                    5
        auto-window-vscroll               nil               ;; Disable auto height of line
        truncate-partial-width-windows    t)                ;; Disable line truncate
  ;; Inhibit
  (setq frame-resize-pixelwise            t                 ;; Don't restricting the frame size to an integer multiple of the font size in the frame.
        inhibit-startup-screen            t
        inhibit-startup-echo-area-message t
        inhibit-compacting-font-caches    t)                ;; Don’t compact font caches during GC.
  (defalias 'yes-or-no-p 'y-or-n-p)                         ;; Use y-or-n instead of yes-or-no
  ;; Frame
  (dolist (frame-setting--var
           '((font . "Sarasa Mono SC Nerd Font-12")
             (alpha . (95 92))
             ;; (alpha-background . 98)
             ))
    (add-to-list 'default-frame-alist frame-setting--var)))

;; Dashboard (Melpa)
(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  (add-to-list 'after-make-frame-functions #'dashboard-in-new-frame)
  (use-package dashboard-ls)
  :config
  (setq dashboard-display-icons-p t ;; display icons on both GUI and terminal
        dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
  (setq dashboard-buffer-name "*Dashboard*")
  (setq dashboard-center-content    t
        dashboard-set-navigator     t
        dashboard-set-file-icons    t
        dashboard-set-heading-icons nil)
  (setq dashboard-items '((recents        . 15)
                          (bookmarks      . 15)
                          (ls-directories . 5)
                          (ls-files       . 5)))
  (setq dashboard-projects-switch-function 'ivy-persp-switch-project-action)

  (defun new-dashboard-with-main-persp ()
    "Jump to the dashboard buffer, if doesn't exists create one."
    (interactive)
    (goto-default-persp)
    (switch-to-buffer dashboard-buffer-name)
    (delete-other-windows)
    (dashboard-mode)
    (dashboard-insert-startupify-lists)
    (dashboard-refresh-buffer))

  (defun dashboard-in-new-frame (&optional frame)
    (interactive)
    (with-selected-frame (or frame (selected-frame))
      (new-dashboard-with-main-persp)))

  (dashboard-setup-startup-hook)
  :general
  ("<f9>"  #'new-dashboard-with-main-persp
   "C-c d" #'new-dashboard-with-main-persp))

;; Doom Modeline (Melpa)
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :after (nerd-icons)
  :defer t
  :config
  (setq doom-modeline-vcs-max-length             40
        doom-modeline-height                     25
        doom-modeline-bar-width                  7
        doom-modeline-total-line-number          t
        doom-modeline-buffer-file-name-style     'truncate-nil
        doom-modeline-github                     t
        doom-modeline-indent-info                t
        doom-modeline-github-interval            (* 30 60)
        doom-modeline-project-detection          'projectile
        doom-modeline-display-default-persp-name t))

;; doom-themes (Melpa)
(use-package doom-themes
  :config
  (load-theme 'doom-snazzy t))

;; minimap (GNU)
(use-package minimap
  :hook
  (after-init . minimap-mode)
  :config
  (add-to-list 'aw-ignored-buffers " *MINIMAP*")
  (setq minimap-window-location 'right
        minimap-hide-scroll-bar t
        minimap-update-delay 0.01
        minimap-width-fraction 0.10
        minimap-minimum-width 20
        minimap-automatically-delete-window 'visible)
  :general
  ("<f8>" #'minimap-mode))


(provide 'init-ui)

;;; init-ui.el ends here
