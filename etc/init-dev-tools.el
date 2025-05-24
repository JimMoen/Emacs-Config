;;; init-dev-tools.el --- Development Tools. -*- lexical-binding: t -*-

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
    :after magit
    :ensure-system-package
    (delta . git-delta)
    ;; (`command-name` . `package-name`)
    :hook
    (magit-mode . magit-delta-mode)
    (magit-mode . (lambda ()
                    (display-line-numbers-mode -1))))

  :after
  nerd-icons

  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons)

  :config
  (setq magit-status-margin                '(t age-abbreviated   magit-log-margin-width t 25)
        magit-refs-margin                  '(t age-abbreviated   magit-log-margin-width t 25)
        magit-reflog-margin                '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 25)
        magit-log-margin                   '(t age-abbreviated   magit-log-margin-width t 25)
        magit-cherry-margin                '(t age-abbreviated   magit-log-margin-width t 25)
        magit-section-visibility-indicator '("…" . t)
        magit-auto-revert-mode             t
        magit-log-color-graph-limit        512
        magit-log-section-commit-count     75
        magit-section-disable-line-numbers nil)

  (setq magit-blame-styles
        '((headings
           (heading-format . "  %C %-18a%f %-80s  %H\n")
           (show-message . t))
          (highlight
           (highlight-face . magit-blame-highlight))))

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
  :custom (diff-hl-draw-borders nil)
  :custom-face
  (diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
  (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector (if (eq system-type 'gnu/linux) #b11111100 #b11100000))
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

;; git historic version view
;; git-timemachine (Melpa)
(use-package git-timemachine)

;; Project Management
;; projectile (Melpa)
(use-package projectile
  :hook
  (prog-mode . projectile-mode)
  :config
  (setq projectile-completion-system 'ivy
        projectile-auto-discover nil
        projectile-track-known-projects-automatically nil)
  :bind
  (("C-x C-b" . projectile-ibuffer))
  (:map projectile-command-map
        ("r"     . nil)
        ("P"     . projectile-discover-projects-in-search-path))
  :bind-keymap
  ("C-x p" . projectile-command-map))

;; persp-mode to managment projcet buffers (Melpa)
(require-all-elisp-in-directory "etc/editor-layouts")

;; file structure tree
;; treemacs (Melpa)
(use-package treemacs
  :config
  (treemacs-follow-mode)
  (treemacs-project-follow-mode)

  (use-package treemacs-projectile
    :after (treemacs projectile))

  (use-package treemacs-nerd-icons
    ;; MUST after lsp-treemacs, otherwise treemacs icons would be theme "Default"
    ;; https://github.com/rainstormstudio/treemacs-nerd-icons/issues/1
    :after (treemacs lsp-treemacs nerd-icons)
    :config
    (treemacs-load-theme "nerd-icons"))

  (use-package treemacs-magit
    :after (treemacs magit))

  ;; treemacs-perspective if you use perspective.el vs. persp-mode
  (use-package treemacs-persp
    :after (treemacs persp-mode) ;; persp-mode
    :config (treemacs-set-scope-type 'Perspectives))

  (setq treemacs-width 60)

  :general
  (:prefix "C-x"
           "4t" 'treemacs
           "t"  'treemacs-select-window))

;; company-mode (Melpa)
;; complete framework
(use-package company
  :init
  (setq company-backends '((company-capf
                            company-files    ;; files & directory
                            company-keywords ;; keywords
                            company-yasnippet)
                           (company-abbrev
                            company-dabbrev
                            company-dabbrev-code)
                           (company-ispell
                            company-restclient)))
  (use-package company-box
    :hook (company-mode . company-box-mode))

  :hook
  (after-init . global-company-mode)

  :custom
  (company-dabbrev-ignore-case    nil)
  (company-dabbrev-downcase       nil)
  (company-dabbrev-char-regexp    "[A-Za-z-_\\.'/]")
  (company-dabbrev-ignore-buffers "\\`[ *]\\|\\.PDF\\'")
  (company-dabbrev-other-buffers  t)

  :config
  (setq company-idle-delay            0.5
        company-minimum-prefix-length 1
        company-require-match         nil
        company-selection-wrap-around t
        company-show-quick-access     t)

  (use-package company-tabnine
    :after company
    :init
    (defvar company-mode/enable-tabnine t
      "Enable tabnine for all backends.")

    (defvar company-backend/elisp '(company-elisp :with company-tabnine) ;; Do not add `:with company-tabnine` in company-backends alist
      "Company backend for `elisp'.")

    (defvar company-mode/disable-tabnine-backends-alist (cons company-backend/elisp '())
      "Disable tabnine for specific backends.")

    :config
    (with-eval-after-load 'company-tabnine
      (defun company-backend-with-tabnine (backend)
        "Add with `tabnine' to company backend."
        (if (or (not company-mode/enable-tabnine)
                (member backend company-mode/disable-tabnine-backends-alist)
                (and (listp backend)
                     (member 'company-tabnine (member ':with backend))))
            backend
          (progn
            (dolist (delq--var '(:with company-tabnine))
              (delq delq--var (if (consp backend) backend (list backend))))
            (append backend '(:with company-tabnine)))))
      (setq company-backends (mapcar #'company-backend-with-tabnine company-backends))))

  (setq company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode))
  (add-to-list 'company-transformers #'delete-dups)

  :bind
  (:map company-active-map
        ("C-h"     . nil)
        ("C-x h"   . company-show-doc-buffer)
        ("C-w"     . nil)
        ("C-x w"   . company-show-location)
        ([tab]     . company-complete-common-or-cycle)
        ([backtab] . company-select-previous-or-abort)))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook
  (prog-mode . copilot-mode)
  :config
  (setq copilot-max-char                      -1
        copilot-indent-offset-warning-disable t)
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))

  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

;; prescient (Melpa)
;; sorting and filtering for Emacs.
(use-package prescient
  :hook (after-init . prescient-persist-mode)
  :straight t
  :init
  ;; for ivy support (Melpa)
  (use-package ivy-prescient
    :straight t
    :after counsel
    :hook (ivy-mode . ivy-prescient-mode)
    :config
    (setq ivy-prescient-enable-filtering nil))
  ;; for company support (Melpa)
  (use-package company-prescient
    :straight t
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
  (setq flycheck-emacs-lisp-load-path 'inherit)
  :config
  (use-package sideline-flycheck
    :init
    (defvar sideline-backends-right '())
    :hook
    ((flycheck-mode . sideline-flycheck-setup)
     (flycheck-mode . sideline-mode))
    :config
    (with-eval-after-load 'flycheck
      (add-to-list 'sideline-backends-right 'sideline-flycheck))))

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
  :init
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  ;; emacs-lsp-booster
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  ;; end emacs-lsp-booster

  (defvar my/disabled-lsp-major-modes
    '(emacs-lisp-mode
      lisp-mode
      makefile-gmake-mode
      hcl-mode
      pkgbuild-mode
      protobuf-mode
      qml-mode
      direnv-envrc-mode))

  (defun disable-lsp-in-modes (select-major-mode)
    "Add the `SELECT-MAJOR-MODE' in the `my/disabled-lsp-major-modes' .
If already in it, do nothing."
    (if (member major-mode my/disabled-lsp-major-modes)
        my/disabled-lsp-major-modes
      (push select-major-mode my/disabled-lsp-major-modes)))

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
         lsp--binding-descriptions)))))

  :commands
  (lsp)
  :hook
  ((prog-mode . (lambda ()
                  (unless
                      (or (apply 'derived-mode-p my/disabled-lsp-major-modes)
                          (string-match (rx "_build/") (buffer-file-name)))
                    (lsp-deferred))))
   (lsp-mode . my/lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-use-plists                     t)
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
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
        lsp-enable-symbol-highlighting     nil
        lsp-semantic-tokens-enable         t
        lsp-enable-relative-indentation    nil)
  (setq lsp-log-io                         nil
        lsp-log-max                        t)
  (setq lsp-headerline-breadcrumb-enable   nil
        lsp-lens-enable                    nil)
  (setq lsp-keep-workspace-alive           nil
        lsp-restart                        'interactive
        lsp-file-watch-threshold           20000
        lsp-auto-guess-root                t
        lsp-enable-dap-auto-configure      t
        lsp-diagnostics-provider           :auto
        lsp-completion-provider            :none
        lsp-idle-delay                     0.5
        lsp-eldoc-enable-hover             nil
        lsp-signature-auto-activate        t              ;; show function signature
        lsp-signature-doc-lines            2              ;; but dont take up more lines
        lsp-auto-execute-action            t
        lsp-signature-render-documentation nil)
  (setq lsp-modeline-code-actions-enable   nil
        lsp-modeline-diagnostics-enable    nil))

;; lsp-ui (Melpa)
(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (use-package lsp-treemacs
    :after (treemacs lsp-mode))
  (use-package sideline-lsp
    :after (lsp-mode sideline)
    :hook (lsp-mode . sideline-mode)
    :config
    (with-eval-after-load  'lsp-mode
      (add-to-list 'sideline-backends-right 'sideline-lsp)))
  (setq lsp-ui-doc-delay                   0.5
        lsp-ui-doc-enable                  t
        lsp-ui-doc-show-with-mouse         t
        lsp-ui-doc-show-with-cursor        t
        lsp-ui-doc-max-height              50
        lsp-ui-sideline-enable             nil
        lsp-ui-sideline-show-code-actions  t
        lsp-ui-sideline-show-hover         t
        lsp-ui-sideline-show-diagnostics   t)
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

;; apheleia (Melpa)
(use-package apheleia)


(provide 'init-dev-tools)

;;; init-dev-tools.el ends here
