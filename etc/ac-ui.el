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
  (progn (setq JimMoen/frame-settings '((font . "Sarasa Mono SC-12")
                                        (alpha . (95 93))))
         (dolist (frame-set-var JimMoen/frame-settings)
           (push frame-set-var default-frame-alist))))

;; all-the-icons (Melpa)
(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor            0.95
        all-the-icons-material-scale-factor   1.00
        all-the-icons-fileicon-scale-factor   0.80
        all-the-icons-wicon-scale-factor      0.65
        all-the-icons-octicon-scale-factor    0.95
        all-the-icons-faicon-scale-factor     0.95
        all-the-icons-alltheicon-scale-factor 1.00)
  (setq all-the-icons-color-icons             t))

;; Dashboard (Melpa)
(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  (add-to-list 'after-make-frame-functions #'dashboard-in-new-frame)
  (use-package dashboard-ls)
  :config
  (setq dashboard-buffer-name "*Dashboard*")
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

  (setq dashboard-set-navigator      t
        dashboard-set-file-icons     t
        dashboard-set-heading-icons  t)
  (setq dashboard-items '((recents        . 10)
                          (projects       . 10)
                          (ls-directories . 5)
                          (ls-files       . 5)))
  (setq dashboard-projects-switch-function 'ivy-persp-switch-project-action)

  (dashboard-setup-startup-hook)
  :general
  ("<f9>"  #'new-dashboard-with-main-persp
   "C-c d" #'new-dashboard-with-main-persp))

;; Doom Modeline (Melpa)
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :defer t
  :config
  (setq
   doom-modeline-vcs-max-length             30
   doom-modeline-mode-height                20
   doom-modeline-bar-width                  3
   doom-modeline-window-width-limit         fill-column
   doom-modeline-buffer-file-name-style     'truncate-nil
   doom-modeline-github                     t
   doom-modeline-github-interval            (* 30 60)
   doom-modeline-project-detection          'projectile
   doom-modeline-display-default-persp-name t))

;; kaolin-themes (Melpa)
(use-package kaolin-themes
  :config
  ;; If t, enable italic style in comments.
  (setq kaolin-themes-italic-comments t)
  ;; When t, will display colored hl-line style instead monochrome.
  (setq kaolin-themes-hl-line-colored t)
  ;; Enable distinct background for fringe and line numbers.
  (setq kaolin-themes-distinct-fringe t)
  ;; Enable distinct colors for company popup scrollbar.
  (setq kaolin-themes-distinct-company-scrollbar t)
  ;; Show git-gutter indicators as solid lines
  (setq kaolin-themes-git-gutter-solid t)
  ;; (load-theme 'kaolin-dark t)
  (load-theme 'kaolin-galaxy t))


(provide 'ac-ui)

;;; ac-ui.el ends here
