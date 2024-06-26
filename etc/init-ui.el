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
  ;; Scroll
  (setq scroll-step                       1                 ;; Friendly and smoothly scroll
        scroll-margin                     10
        scroll-conservatively             101
        scroll-up-aggressively            0.0
        scroll-down-aggressively          0.0
        scroll-preserve-screen-position   'always
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

;; nerd-icons (Melpa)
(use-package nerd-icons
  :config
  (defun update-alist (alist-symbol rep-alist)
    "Update the alist specified by ALIST-SYMBOL with entries from REP-ALIST.
If a key from REP-ALIST is present in the alist referred to by ALIST-SYMBOL,
its value will be updated. If the key is not present, the entry will be added."
    (let ((alist (symbol-value alist-symbol)))
      (dolist (rep rep-alist)
        (let ((key (car rep))
              (value (cdr rep)))
          (if (assoc key alist)
              (setcdr (assoc key alist) value)
            (setq alist (cons rep alist)))))
      (set alist-symbol alist)))

  (update-alist 'nerd-icons-extension-icon-alist
                '(
                  ("ini"        nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  ("properties" nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)

                  ("json"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  ("jsonl"      nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  ("cson"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  ("yml"        nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dyellow)
                  ("yaml"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dyellow)
                  ("toml"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-orange)
                  ("conf"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dorange)

                  ("tscn"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-orange)
                  ("tres"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-orange)

                  ("config"     nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dyellow)
                  ))
  (update-alist 'nerd-icons-mode-icon-alist
                '(
                  (Custom-mode  nerd-icons-sucicon "nf-seti-settings")

                  (conf-mode    nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-lyellow)
                  (json-mode    nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  (json-ts-mode nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  (jsonian-mode nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  (yaml-mode    nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dyellow)
                  (yaml-ts-mode nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dyellow)
                  (toml-mode    nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-orange)
                  (toml-ts-mode nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-orange)
                  ))
  (update-alist 'nerd-icons-regexp-icon-alist
                '(
                  ("^rebar3.crashdump$" nerd-icons-devicon "nf-dev-erlang" :face nerd-icons-lred)
                  ("^security"          nerd-icons-faicon  "nf-fa-lock"    :face nerd-icons-lcyan)
                  ("^rebar3$"           nerd-icons-devicon "nf-dev-erlang" :face nerd-icons-orange)
                  ))
  (setq nerd-icons-font-family "Sarasa Gothic SC Nerd Font"
        nerd-icons-scale-factor 0.8))

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


(provide 'init-ui)

;;; init-ui.el ends here
