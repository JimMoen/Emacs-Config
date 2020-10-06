;;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;;; File: bb-dev-complete.el
;;;; Auth: JimMoen
;;;; Code:

;;;; ####################################################### Company
;;;;                                              ========== Company (None Built-in)
(use-package company
  :hook
  ('after-init . 'global-company-mode)
  :config
  (setq company-idle-delay                0
        company-tooltip-align-annotations t
        company-require-match             nil
        company-dabbrev-code-everywhere   t
        company-dabbrev-other-buffers     nil
        company-dabbrev-ignore-case       nil
        company-dabbrev-downcase          nil
        company-show-numbers              t
        company-minimum-prefix-length     1)
  (setq company-backends '(company-capf company-files
                                        (company-dabbrev-code company-gtags company-etags company-keywords)))
  :bind
  (:map company-active-map
        ("M-n"     . nil)
        ("C-n"     . company-select-next)
        ("M-p"     . nil)
        ("C-p"     . company-select-previous)
        ("C-h"     . nil)
        ("C-x h"   . company-show-doc-buffer)
        ("C-w"     . nil)
        ("C-x w"   . company-show-location)
        ([tab]     . company-complete-common-or-cycle)
        ([backtab] . company-select-previous-or-abort)))

;;;;                                              ========== lsp (None Built-in)
(use-package lsp-mode
  :hook
  (('python-mode . 'lsp)
   ('lsp-mode    . 'lsp-enable-which-key-integration))
  :commands
  (lsp))

;;;;                                              ========== lsp-ui (None Built-in)
(use-package lsp-ui
  :commands
  (lsp-ui-mode))

;;;;                                              ========== lsp-ivy (None Built-in)
(use-package lsp-ivy
  :commands
  (lsp-ivy-workspace-symbol))

;; TODO

;;;; ####################################################### ;;;;
(provide 'bb-dev-complete)
;;;; bb-dev-complete.el ends here
