;;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;;; File: ac-ui.el
;;;; Auth: JimMoen
;;;; Code:

;;;; ####################################################### Built-in Features
;;;;                                              ========== Eamcs Built-in features
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



;;;; ####################################################### Modeline
;;;;                                              ========== Doom Modeline (None Built-in)
(use-package doom-modeline
  :hook
  ('after-init . 'doom-modeline-mode)
  :init
  (setq doom-modeline-icon nil)                           ;; Do not use icon
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



;;;; ####################################################### Color Theme
;;;;                                              ========== color-theme-sanityinc-tomorrow (None Built-in)
(use-package color-theme-sanityinc-tomorrow
  :init
  (setq custom-safe-themes t)
  (load-theme 'sanityinc-tomorrow-night))



;;;; ####################################################### ;;;;
(provide 'ac-ui)
;;;; ac-ui.el ends here
