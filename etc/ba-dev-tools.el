;;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;;; File: ba-dev-tools.el
;;;; Auth: JimMoen
;;;; Code:

;;;; ####################################################### Git
;;;;                                              ========== Magit (None Built-in)
(use-package magit
  :bind
  (("C-x g" . magit-status)))
;; TODO

;;;;                                              ========== Projectile (None Built-in)
(use-package projectile
  :hook
  ('prog-mode    . 'projectile-mode)
  :bind
  (("C-x C-b" . projectile-ibuffer))                      ;; Working with persp-mode-projectile-bridge-mode
  (:map projectile-command-map
        ("r"     . nil))
  :bind-keymap
  ("C-c p" . projectile-command-map))

;;;;                                              ========== Counsel Projectile (None Built-in)
(use-package counsel-projectile
  :after counsel projectile
  :hook
  ('after-init . 'counsel-projectile-mode))

;;;;                                              ========== persp-mode-projectile-bridge (None Built-in)
(use-package persp-mode-projectile-bridge
  :ensure nil
  :load-path "site-lisp/persp-mode-projectile-bridge"
  :config
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
  ('persp-mode . 'persp-mode-projectile-bridge-mode)
  ('persp-mode-projectile-bridge-mode . (lambda ()
                                          (if persp-mode-projectile-bridge-mode
                                              (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                                            (persp-mode-projectile-bridge-mode)
                                            (persp-mode-projectile-bridge-kill-perspectives)))))



;;;; ####################################################### ;;;;
(provide 'ba-dev-tools)
;;;; ba-dev-tools.el ends here
