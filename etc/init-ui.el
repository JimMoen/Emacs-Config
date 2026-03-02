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
  (setq auto-window-vscroll               nil               ;; Disable auto height of line
        truncate-partial-width-windows    t)                ;; Disable line truncate
  ;; Inhibit
  (setq frame-resize-pixelwise            t                 ;; Don't restricting the frame size to an integer multiple of the font size in the frame.
        inhibit-startup-screen            t
        inhibit-startup-echo-area-message t
        inhibit-compacting-font-caches    t)                ;; Don’t compact font caches during GC.
  (defalias 'yes-or-no-p 'y-or-n-p)                         ;; Use y-or-n instead of yes-or-no

  (add-to-list 'default-frame-alist '(alpha . (95 92)))
  ;; Frame
  (set-face-attribute 'default nil :font "Sarasa Mono SC Nerd Font-11")
  (defvar user/cjk-font "Sarasa Mono SC Nerd Font"
    "Default font for CJK characters.")

  (defvar user/latin-font "Sarasa Mono SC Nerd Font"
    "Default font for Latin characters.")

  (defvar user/unicode-font "Iosevka Nerd Font Mono"
    "Default font for Unicode characters, including emojis.")

  (defvar user/font-size 18
    "Default font size in px.")

  (defvar user/standard-fontset
    (create-fontset-from-fontset-spec standard-fontset-spec)
    "Standard fontset for user.")

  ;; Ensure user/standard-fontset gets used for new frames.
  (add-to-list 'default-frame-alist (cons 'font user/standard-fontset))
  (add-to-list 'initial-frame-alist (cons 'font user/standard-fontset))

  (defun user/set-font ()
    "Set Unicode, Latin and CJK font for user/standard-fontset."
    ;; Unicode font.
    (set-fontset-font user/standard-fontset 'unicode
                      (font-spec :family user/unicode-font)
                      nil 'prepend)
    ;; Latin font.
    ;; Only specify size here to allow text-scale-adjust work on other fonts.
    (set-fontset-font user/standard-fontset 'latin
                      (font-spec :family user/latin-font :size user/font-size)
                      nil 'prepend)
    ;; CJK font.
    (dolist (charset '(kana han cjk-misc hangul kanbun bopomofo))
      (set-fontset-font user/standard-fontset charset
                        (font-spec :family user/cjk-font)
                        nil 'prepend))
    ;; Special settings for certain CJK puncuation marks.
    ;; These are full-width characters but by default uses half-width glyphs.
    (dolist (charset '((#x2018 . #x2019)    ;; Curly single quotes "‘’"
                       (#x201c . #x201d)))  ;; Curly double quotes "“”"
      (set-fontset-font user/standard-fontset charset
                        (font-spec :family user/cjk-font)
                        nil 'prepend)))
  ;; Apply changes.
  (user/set-font)
  ;; For emacsclient.
  (add-hook 'before-make-frame-hook #'user/set-font))

(use-package ultra-scroll
  :ensure (:host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively      101 ; important!
        scroll-margin              0
        ultra-scroll-gc-percentage 0.1
        ultra-scroll-gc-idle-time  0.2
        ultra-scroll-hide-cursor   0.5)
  :config
  (ultra-scroll-mode 1))

;; Dashboard (Melpa)
(use-package dashboard
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
    (when (bound-and-true-p persp-mode)
      (goto-default-persp))
    (require 'dashboard-ls nil t)
    (switch-to-buffer dashboard-buffer-name)
    (delete-other-windows)
    (dashboard-mode)
    (dashboard-insert-startupify-lists)
    (dashboard-refresh-buffer))

  (defun dashboard-in-new-frame (&optional frame)
    (interactive)
    (with-selected-frame (or frame (selected-frame))
      (new-dashboard-with-main-persp)))

  ;; For emacsclient -c: show dashboard after server fully sets up the frame
  (add-hook 'server-after-make-frame-hook #'dashboard-in-new-frame)

  ;; Auto-open dashboard at startup after all packages are ready.
  ;; Depth 90 ensures this runs AFTER persp-mode activation (depth 0).
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              (require 'dashboard-ls nil t)
              (when (bound-and-true-p persp-mode)
                (goto-default-persp))
              (unless (daemonp)
                (dashboard-insert-startupify-lists)
                (when (get-buffer dashboard-buffer-name)
                  (switch-to-buffer dashboard-buffer-name))))
            90)
  :general
  ("<f9>"  #'new-dashboard-with-main-persp
   "C-c d" #'new-dashboard-with-main-persp))

;; dashboard-ls extends dashboard with ls-directories/ls-files items
(use-package dashboard-ls)

;; show-inactive-region (Melpa)
(use-package show-inactive-region
  :defer nil
  :hook
  (prog-mode . show-inactive-region-mode)
  :config
  (setq show-inactive-region-face-dynamic-factor 0.05
        show-inactive-region-fade-delay 0.3
        show-inactive-region-fade-out   0.1))

;; beacon (Melpa)
(use-package beacon
  :defer nil
  :config
  (setq beacon-blink-when-focused                t
        beacon-blink-when-window-changes         t
        beacon-blink-when-point-moves-vertically t
        beacon-blink-duration                    0.3
        beacon-size                              50)
  (beacon-mode 1))

;; Doom Modeline (Melpa)
(use-package doom-modeline
  :after (nerd-icons)
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-vcs-max-length             40
        doom-modeline-height                     25
        doom-modeline-bar-width                  7
        doom-modeline-unicode-fallback           t
        doom-modeline-total-line-number          t
        doom-modeline-buffer-file-name-style     'truncate-nil
        doom-modeline-github                     t
        doom-modeline-indent-info                t
        doom-modeline-github-interval            (* 30 60)
        doom-modeline-project-detection          'projectile
        doom-modeline-display-default-persp-name t
        doom-modeline-hud                        t))

;; doom-themes (Melpa)
(use-package doom-themes
  :ensure (:host github :repo "doomemacs/themes" :branch "master")
  :config
  (setcdr (assoc 'gnus-group-news-low-empty doom-themes-base-faces)
          '(:inherit 'gnus-group-mail-1-empty :weight 'normal))
  (load-theme 'doom-dracula t)
  (custom-set-faces
   '(font-lock-variable-name-face ((t (:foreground "#ff72a4"))))))


(provide 'init-ui)

;;; init-ui.el ends here
