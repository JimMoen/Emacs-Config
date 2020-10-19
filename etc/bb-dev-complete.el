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
  (setq company-backends '(company-semantic
                           company-capf
                           company-files
                           company-elisp
                           (company-dabbrev
                            company-dabbrev-code
                            company-gtags
                            company-etags
                            company-keywords)))
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



;;;; ####################################################### Language Server Protocal
;;;;                                              ========== lsp (None Built-in)
(use-package lsp-mode
  :commands
  (lsp)
  :hook
  ((prog-mode . (lambda ()
                  (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                    (lsp-deferred))))
   (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "C-c l")
  :init
  (setq lsp-auto-configure                 nil
        lsp-enable-on-type-formatting      t
        lsp-completion-enable              nil
        lsp-enable-indentation             t
        lsp-enable-file-watchers           t
        lsp-enable-imenu                   t
        lsp-enable-text-document-color     t
        lsp-enable-links                   t
        lsp-enable-xref                    t
        lsp-enable-snippet                 t
        lsp-enable-folding                 nil
        lsp-enable-symbol-highlighting     nil
        lsp-enable-relative-indentation    nil
        lsp-enable-semantic-highlighting   nil)
  :config
  (setq lsp-log-io                         nil
        lsp-log-max                        t)
  (setq lsp-keep-workspace-alive           nil
        lsp-restart                        'interactive
        lsp-enable-dap-auto-configure      nil
        lsp-completion-provider            :capf
        lsp-diagnostics-provider           :flycheck      ;; prefer `flycheck'
        lsp-idle-delay                     0.05
        lsp-eldoc-enable-hover             nil
        lsp-eldoc-render-all               nil
        lsp-signature-auto-activate        t              ;; show function signature
        lsp-signature-doc-lines            2              ;; but dont take up more lines
        lsp-auto-execute-action            nil
        lsp-signature-render-documentation t))

;;;;                                              ========== lsp-ui (None Built-in)
(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-delay         0.05)
  :bind
  (:map lsp-mode-map
        (("C-c l i"            . lsp-ui-imenu))))

;;;;                                              ========== lsp-ivy (None Built-in)
(use-package lsp-ivy
  :commands
  (lsp-ivy-workspace-symbol))



;;;; ####################################################### ;;;;
(provide 'bb-dev-complete)
;;;; bb-dev-complete.el ends here
