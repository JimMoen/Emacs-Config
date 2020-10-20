;;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;;; File: ba-dev-tools.el
;;;; Auth: JimMoen
;;;; Code:

;;;; ####################################################### Git
;;;;                                              ========== Magit (None Built-in)
(use-package magit
  :config
  (setq magit-status-margin            '(t age-abbreviated   magit-log-margin-width t 18)
        magit-refs-margin              '(t age-abbreviated   magit-log-margin-width t 18)
        magit-reflog-margin            '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)
        magit-log-section-commit-count 50)
  (setq magit-status-initial-section
        '(((unpulled . "..@{upstream}") (status))
          ((untracked) (status))
          ((unstaged) (status))
          1))
  :bind
  (("C-x g" . magit-status)))

(use-package keychain-environment
  :demand t
  :hook
  (after-init . keychain-refresh-environment))



;;;; ####################################################### Project Management
;;;;                                              ========== Projectile (None Built-in)
(use-package projectile
  :hook
  (prog-mode . projectile-mode)
  :config
  (setq projectile-completion-system 'ivy)
  :bind
  (("C-x C-b" . projectile-ibuffer))                      ;; Working with persp-mode-projectile-bridge-mode
  (:map projectile-command-map
        ("r"     . nil))
  :bind-keymap
  ("C-x p" . projectile-command-map))

;;;;                                              ========== Counsel Projectile (None Built-in)
(use-package counsel-projectile
  :after counsel projectile
  :hook
  (after-init . counsel-projectile-mode))

;;;;                                              ========== persp-mode-projectile-bridge (None Built-in)
(use-package persp-mode-projectile-bridge
  :ensure nil
  :load-path "site-lisp/persp-mode-projectile-bridge"
  :config
  (setq persp-mode-projectile-bridge-persp-name-prefix "[P] ")
  (defun persp-mode-projectile-bridge-add-new-persp-save-to-file (name)
    (let ((persp (persp-get-by-name name *persp-hash* :nil)))
      (if (eq :nil persp)
          (prog1
              (setq persp (persp-add-new name))
            (when persp
              (set-persp-parameter 'persp-mode-projectile-bridge t persp)
              (set-persp-parameter 'dont-save-to-file nil persp)
              (persp-add-buffer (projectile-project-buffers)
                                persp nil nil)))
        persp)))
  (advice-add 'persp-mode-projectile-bridge-add-new-persp-save-to-file :override #'persp-mode-projectile-bridge-add-new-persp)
  :hook
  (persp-mode . persp-mode-projectile-bridge-mode)
  (persp-mode-projectile-bridge-mode . (lambda ()
                                          (if persp-mode-projectile-bridge-mode
                                              (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                                            (persp-mode-projectile-bridge-mode)
                                            (persp-mode-projectile-bridge-kill-perspectives)))))



;;;; ####################################################### Code Check
;;;;                                              ========== flycheck (None Built-in)
(use-package flycheck
  :hook
  (after-init . global-flycheck-mode))



;;;; ####################################################### ;;;;
(provide 'ba-dev-tools)
;;;; ba-dev-tools.el ends here
