;;; ba-dev-tools.el --- Development Tools. -*- lexical-binding: t -*-

;; Copyright (C) 2021  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: development


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

;; Basic tools for Development.

;;; Code:

;; VCS-Git
;; Magit (Melpa)
(use-package magit
  :init
  (use-package magit-delta
    :ensure-system-package
    (delta . git-delta)
    ;; (`command-name` . `package-name`)
    :hook
    (magit-mode . magit-delta-mode))
  :config
  (setq magit-status-margin                '(t age-abbreviated   magit-log-margin-width t 25)
        magit-refs-margin                  '(t age-abbreviated   magit-log-margin-width t 25)
        magit-reflog-margin                '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 25)
        magit-log-margin                   '(t age-abbreviated   magit-log-margin-width t 25)
        magit-cherry-margin                '(t age-abbreviated   magit-log-margin-width t 25)
        magit-section-visibility-indicator '("…" . t)
        magit-auto-revert-mode             t
        magit-log-section-commit-count     75)

  (setq magit-blame-styles
        '((margin
           (margin-format "%C %-15a%f" "  %s" "    %H")
           (margin-width . 50)
           (margin-face . magit-blame-margin)
           (margin-body-face . magit-blame-dimmed)
           (show-message . t))
          (headings
           (heading-format . "  %C %-25a %-65s %H\n"))))

  (put 'magit-log-mode 'magit-log-default-arguments
       '("--graph" "-n256" "--color" "--decorate"))

  (setq magit-status-initial-section
        '(((unpulled . "..@{upstream}") (status))
          ((untracked) (status))
          ((unstaged) (status))
          1))
  (set-face-attribute 'magit-diff-file-heading nil :foreground "#f57bae" :weight 'bold)
  (defun my/change-commit-author (arg)
    "Change the commit author during an interactive rebase in Magit.
With a prefix argument, insert a new change commit author command
even when there is already another rebase command on the current
line.  With empty input, remove the change commit author action
on the current line, if any."
    (interactive "P")
    (let ((author
           (magit-transient-read-person "Select a new author for this commit"
                                        nil
                                        nil)))
      (git-rebase-set-noncommit-action
       "exec"
       (lambda (_) (if author
                       (format "git commit --amend --author='%s'" author)
                     ""))
       arg)))
  :general
  ("C-x g" 'magit-status
   "C-x G" 'magit-blame-addition)
  ;; (:keymaps 'magit-mode-map)
  (:keymaps 'git-rebase-mode-map
            "h" 'my/change-commit-author))

;; diff unstaged
;; diff-hl (Melpa)
(use-package diff-hl
  :hook
  (after-init . global-diff-hl-mode)
  :init
  (setq diff-hl-flydiff-mode t
        diff-hl-margin-mode  t))

;; git historic version view
;; git-timemachine (Melpa)
(use-package git-timemachine)

;; Project Management
;; projectile (Melpa)
(use-package projectile
  :hook
  (prog-mode . projectile-mode)
  :config
  (setq projectile-completion-system 'ivy)
  :bind
  (("C-x C-b" . projectile-ibuffer))
  (:map projectile-command-map
        ("r"     . nil))
  :bind-keymap
  ("C-x p" . projectile-command-map))

;; counsel-projectile (Melpa)
(use-package counsel-projectile
  :after counsel projectile
  :hook
  (after-init . counsel-projectile-mode))

;; persp-mode to managment projcet buffers (Melpa)
(require-all-elisp-in-directory "etc/editor-layouts")

;; company-mode (Melpa)
;; complete framework
(use-package company
  :init
  (setq company-backends '((company-tabnine
                            company-dabbrev
                            company-dabbrev-code
                            company-keywords
                            company-files
                            company-ispell
                            company-capf)))
  :hook
  (after-init . global-company-mode)
  (emacs-lisp-mode . (lambda ()
                       (require 'company-elisp)
                       (if (and (listp company-backends) (member 'company-elisp company-backends))
                           company-backends
                         (push 'company-elisp company-backends))))
  :config
  (setq company-idle-delay                0.15
        company-minimum-prefix-length     1
        company-require-match             nil
        company-selection-wrap-around     t
        company-show-quick-access         t
        company-tooltip-align-annotations t
        company-dabbrev-ignore-case       nil
        company-dabbrev-downcase          nil
        company-dabbrev-char-regexp       "[A-Za-z-_\\.'/]"
        company-dabbrev-ignore-buffers    "\\`[ *]\\|\\.pdf\\'"
        company-dabbrev-other-buffers     t
        company-show-quick-access         t)
  (setq company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode))

  (with-no-warnings
    (defvar company-mode/enable-yas t
      "Enable yasnippet for all backends.")

    (with-eval-after-load 'yasnippet
      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))

      (setq company-backends (mapcar #'company-backend-with-yas company-backends))

      (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq command 'prefix)
            (when-let ((prefix (funcall fun 'prefix)))
              (unless (memq (char-before (- (point) (length prefix))) '(?. ?> ?\())
                prefix))
          (funcall fun command arg)))

      (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline))
    (add-to-list 'company-transformers #'delete-dups))
  :bind
  (:map company-active-map
        ("C-h"     . nil)
        ("C-x h"   . company-show-doc-buffer)
        ("C-w"     . nil)
        ("C-x w"   . company-show-location)
        ([tab]     . company-complete-common-or-cycle)
        ([backtab] . company-select-previous-or-abort)))

;; company-tabnine (Melpa)
;; complete with tabnine AI
(use-package company-tabnine)

;; prescient (Melpa)
;; sorting and filtering for Emacs.
(use-package prescient
  :hook (after-init . prescient-persist-mode)
  :init
  ;; for ivy support (Melpa)
  (use-package ivy-prescient
    :after counsel
    :hook (ivy-mode . ivy-prescient-mode)
    :config
    (setq ivy-prescient-enable-filtering nil))
  ;; for company support (Melpa)
  (use-package company-prescient
    :hook (company-mode . company-prescient-mode))
  :config
  (setq prescient-sort-full-matches-first t
        prescient-sort-length-enable      nil))

;; Code Check
;; flycheck (Melpa)
(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit))

;; Code template
;; yasnippet (Melpa)
(use-package yasnippet
  :hook
  (after-init . yas-global-mode)
  :config
  (setq yas-triggers-in-field t
        yas-wrap-around-region t)
  ;; disable yas minor mode map
  (setq yas-minor-mode-map (make-sparse-keymap)))

;; yasnippet-snippets (Melpa)
(use-package yasnippet-snippets
  :after yasnippet)

;; language-server-protocol (Melpa)
(use-package lsp-mode
  :commands
  (lsp)
  :hook
  ((prog-mode . (lambda ()
                  (unless (or
                           (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                           (derived-mode-p 'makefile-gmake-mode))
                    (lsp-deferred))))
   (lsp-mode . my/lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "C-c l")
  :init
  (setq lsp-auto-configure                 t
        lsp-enable-on-type-formatting      t
        lsp-completion-enable              t
        lsp-enable-indentation             t
        lsp-enable-file-watchers           t
        lsp-enable-imenu                   t
        lsp-enable-text-document-color     t
        lsp-enable-links                   t
        lsp-enable-xref                    t
        lsp-enable-snippet                 t
        lsp-enable-folding                 nil
        lsp-enable-symbol-highlighting     t
        lsp-semantic-tokens-enable         t
        lsp-enable-relative-indentation    nil)
  :config
  (setq lsp-log-io                         nil
        lsp-log-max                        t)
  (setq lsp-keep-workspace-alive           nil
        lsp-restart                        'interactive
        lsp-auto-guess-root                t
        lsp-enable-dap-auto-configure      t
        lsp-completion-provider            :none
        lsp-idle-delay                     0.15
        lsp-eldoc-enable-hover             t
        lsp-eldoc-render-all               nil
        lsp-signature-auto-activate        t              ;; show function signature
        lsp-signature-doc-lines            2              ;; but dont take up more lines
        lsp-auto-execute-action            t
        lsp-signature-render-documentation t)

  ;; https://github.com/emacs-lsp/lsp-mode/issues/2932
  (defun lsp-restart ()
    (interactive)
    (lsp-disconnect)
    (setq lsp--session nil)
    (lsp))
  (defun my/lsp-enable-which-key-integration (&optional all-modes)
    "Adds descriptions for `lsp-mode-map' to `which-key-mode' for the current
active `major-mode', or for all major modes when ALL-MODES is t."
    (cl-flet ((which-key-fn (if all-modes
                                'which-key-add-key-based-replacements
                              (apply-partially 'which-key-add-major-mode-key-based-replacements major-mode))))
      (apply
       #'which-key-fn
       (lsp--prepend-prefix
        (cl-list*
         ""    "lsp"
         "w"   "workspaces"
         "F"   "folders"
         "="   "formatting"
         "T"   "toggle"
         "g"   "LSP Peek" ;; swap G <=> g
         "h"   "help"
         "r"   "refactor"
         "a"   "code actions"
         "G"   "LSP Goto"
         lsp--binding-descriptions))))))

;; lsp-ui (Melpa)
(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-headerline-breadcrumb-enable   nil
        lsp-lens-enable                    nil)
  (setq lsp-ui-doc-delay                   0.15
        lsp-ui-doc-enable                  nil
        lsp-ui-doc-show-with-mouse         nil
        lsp-ui-doc-show-with-cursor        nil
        lsp-ui-doc-max-height              50)
  :general
  (:prefix
   lsp-keymap-prefix
   :keymaps
   'lsp-mode-map
   "i"  '(lsp-ui-imenu :wk "lsp-ui-imenu")
   "d"  '(lsp-ui-doc-show :wk "lsp-ui-doc-show")

   "gg" 'lsp-ui-peek-find-definitions
   "gi" 'lsp-ui-peek-find-implementation
   "gr" 'lsp-ui-peek-find-references
   "gs" 'lsp-ui-peek-find-workspace-symbol

   "Ga" 'xref-find-apropos
   "Gd" 'lsp-find-declaration
   "Ge" 'lsp-treemacs-errors-list
   "Gg" 'lsp-find-definition
   "Gh" 'lsp-treemacs-call-hierarchy
   "Gi" 'lsp-find-implementation
   "Gr" 'lsp-find-references
   "Gt" 'lsp-find-type-definition))

;; lsp-ivy (Melpa)
(use-package lsp-ivy
  :commands
  (lsp-ivy-workspace-symbol))


(provide 'ba-dev-tools)

;;; ba-dev-tools.el ends here
