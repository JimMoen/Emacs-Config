;;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;;; File: aa-core.el
;;;; Auth: JimMoen
;;;; Code:

;;;; ####################################################### Initialize Package Management System
;;;;                                              ========== Package Archives
(setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
;;;;                                              ========== Initialize packages
(unless (bound-and-true-p package--initialized)           ;; Avoid warnings in 27
  (setq package-check-signature nil)                      ;; Check signature when installing
  (package-initialize))
;;;;                                              ========== Setup 'use-package'
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))
;;;;                                              ========== use-package default args
(setq use-package-always-ensure       t                   ;; Always ensure
      use-package-always-defer        nil                 ;; Never defer
      use-package-always-demand       t                   ;; Always demand
      use-package-expand-minimally    t)                  ;; Be Silent



;;;; ####################################################### Package Management
(use-package emacs
  :ensure nil
  :config
  (setq auth-source-save-behavior nil)
  (setq  my-packages
         '(;; ==============================########## core packages
           ;; [Built-in] None
           ;; [Site]     None
           use-package use-package-ensure-system-package
           no-littering restart-emacs disable-mouse

           ;; ==============================########## base packages
           ;; [Built-in] dired autorevert recentf bookmark ibuffer winner
           ;; [Site]     None
           ivy counsel swiper which-key
           dired-single persp-mode switch-window shackle

           ;; ==============================########## ui packages
           ;; [Built-in] None
           ;; [Site]     None
           doom-modeline color-theme-sanityinc-tomorrow

           ;; ==============================########## base editing
           ;; [Built-in] align display-line-numbers delsel
           ;;            so-long subword whitespace hideshow
           ;; [Site]     None
           wgrep avy rainbow-mode smartparens
           rainbow-delimiters

           ;; ==============================########## other utils
           ;; [Built-in] calendar
           ;; [Site]     english-teacher
           rime cal-china-x youdao-dictionary
           speed-type pdf-tools info-colors

           ;; ==============================########## Dev Core packages
           ;; [Built-in] None
           ;; [Site]     persp-mode-projectile-bridge
           magit keychain-environment projectile counsel-projectile

           ;; ==============================########## Dev Completing ;;TODO
           ;; [Built-in] None
           ;; [Site]     None
           company lsp-mode lsp-ui lsp-ivy
           lsp-python-ms

           ;; ==============================########## Program language packages ;;TODO
           ;; [Built-in] None
           ;; [Site]     None
           yaml-mode lua-mode js2-mode haskell-mode python-mode))

  ;;;; ########## enable some commands
  (progn (setq enabled-functions '(dired-find-alternate-file
                                   narrow-to-region
                                   upcase-region
                                   downcase-region))
         (dolist (want-enabled-function enabled-functions)
           (put want-enabled-function 'disabled nil)))
  ;;;; ########## avoid package.el "custom-set-variable" in custom.el
  (progn (defun my-save-selected-packages (&optional package-list)
           "set and (don't!) save `package-selected-packages' to value."
           (when package-list
             (setq package-selected-packages package-list))
           (unless after-init-time
             (add-hook 'after-init-hook #'package--save-selected-packages)))
         (advice-add 'package--save-selected-packages :override #'my-save-selected-packages)
         (my-save-selected-packages my-packages)))



;;;; ####################################################### Initialize Crucial Package
;;;;                                              ========== ... (None Built-in)
(use-package use-package-ensure-system-package            ;; For system packages. Such as "ripgrep"
  :demand t)

;;;;                                              ========== ... (None Built-in)
(use-package no-littering                                 ;; Built-in packags "recentf" needed
  :demand t)

;;;;                                              ========== ... (None Built-in)
(use-package restart-emacs                                ;; Easy to Restart Emacs
  :demand t)

;;;;                                              ========== ... (None Built-in)
(use-package disable-mouse                                ;; Disable the mouse action
  :demand t
  :hook
  ('after-init . 'global-disable-mouse-mode))



;;;; ####################################################### ;;;;
(provide 'aa-core)
;;;; aa-core.el ends here
