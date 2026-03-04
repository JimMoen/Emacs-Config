;;; init-editing.el --- Easy Editing. -*- lexical-binding: t -*-

;; Copyright (C) 2021  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: Easy Editing


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

;; Easy Editing.

;;; Code:

;; Indent Settings
;; Emacs Built-in Features
(use-package emacs
  :ensure nil
  :custom
  (indent-tabs-mode          nil)                       ;; Use only spaces and no tabs
  (tab-width                 4)                         ;; Tab width set to 4
  (standard-indent           4)                         ;; Default indent sets 4
  :config
  ;; Fix stucking when editing extremely large files.
  ;; See also https://emacs-china.org/t/topic/25811/9
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t
        long-line-threshold 1000
        large-hscroll-threshold 1000
        syntax-wholeline-max 1000)
  (progn
    (blink-cursor-mode t)
    (setq blink-cursor-blinks 5))
  (use-package display-fill-column-indicator
    :ensure nil
    :hook (prog-mode . display-fill-column-indicator-mode)
    :config
    (setq-default fill-column 100)
    (set-face-attribute 'fill-column-indicator nil :foreground "grey40"))

  (defun clear-all-registers ()
    "Clear all registers by setting `register-alist' to nil."
    (interactive)
    (setq register-alist '()))

  :bind
  (("RET"                    .  newline-and-indent)
   ("S-<return>"             .  comment-indent-new-line)))

;; indent-bars (Melpa)
(use-package indent-bars
  :hook ((python-base-mode yaml-mode erlang-mode) . indent-bars-mode)
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                       if_statement with_statement while_statement))))

;; align (Built-in)
(use-package align
  :ensure nil
  :bind
  (("C-c =" . align-regexp)))

;; Intuitional editing
;; Display Line numbers (Built-in)
(use-package display-line-numbers
  :ensure nil
  :hook
  (after-init . global-display-line-numbers-mode)
  :custom
  (display-line-numbers-width 4))

;; Display Column numbers (Built-in)
(use-package simple
  :ensure nil
  :hook
  (after-init . column-number-mode))

;; delsel (Built-in)
(use-package delsel
  :ensure nil
  :hook
  (after-init . delete-selection-mode))

;; so-long (Built-in)
(use-package so-long
  :ensure nil
  :hook
  (after-init . global-so-long-mode))

;; subword (Built-in)
(use-package subword
  :ensure nil
  :hook
  (after-init . global-subword-mode))

;; Whitespace Display (Built-in)
(use-package whitespace
  :ensure nil
  :defer t
  :hook
  (after-init . global-whitespace-mode)
  :config
  ;; Whitespace color corrections.
  (setq whitespace-global-modes
        '(not shell-mode
              help-mode
              magit-mode
              magit-diff-mode
              ibuffer-mode
              dired-mode
              occur-mode))
  (setq whitespace-style
        '(face tabs                     tab-mark
               space-after-tab::space   space-before-tab::space
               indentation::space
               trailing))
  (setq whitespace-display-mappings
        '(;; "tab" char.      Display like "|   ".   Or Display like "\   "
          (tab-mark      9   [124 9]   [92 9])
          ;; " " char.        Display like "·".      Or Display like "_"
          (space-mark    32  [183]     [95])
          ;; "newline" char.  Display like "¬"       Or Display like "¶"
          (newline-mark  10  [172 10]  [182 10])))
  (set-face-attribute 'whitespace-tab      nil :foreground "#444444" :background "#686868")
  (set-face-attribute 'whitespace-empty    nil :foreground "#cd8c95" :background "#8b5f65")
  (set-face-attribute 'whitespace-trailing nil :foreground "#555555" :background "#909D89")
  :bind
  (("C-c t c" . whitespace-cleanup)
   ("C-c t C" . whitespace-cleanup-region)
   ("C-c t r" . whitespace-report)
   ("C-c t R" . whitespace-report-region)))

;; vundo (Melpa)
(use-package vundo
  :config
  (setq undo-limit        67108864    ;; 64mb.
        undo-strong-limit 100663296   ;; 96mb.
        undo-outer-limit  1006632960) ;; 960mb.
  (setq vundo-glyph-alist vundo-ascii-symbols)
  :bind
  (("C-c u" . vundo)))

;; region-occurrences-highlighter (Melpa)
(use-package region-occurrences-highlighter
  :hook
  (prog-mode . region-occurrences-highlighter-mode)
  (org-mode . region-occurrences-highlighter-mode)
  (text-mode . region-occurrences-highlighter-mode)
  :config
  (setq region-occurrences-highlighter-max-size 1000))

;; hl-todo (Melpa)
;; Highlight TODO and similar keywords in comments
(use-package hl-todo
  :config
  (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
        '(("XXX"    . "#FFAABB")
          ("TODO"   . "#FFBB00")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF"))))

;; This assumes you've installed the package via MELPA.
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "=====" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "----" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       ;; "/*" "*/"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; For Code Editing
;; hideshow (Built-in)
(use-package hideshow
  :ensure nil
  :custom
  (hs-hide-comments-when-hiding-all nil)
  :hook
  (prog-mode . hs-minor-mode)
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :weight semi-bold :box (:line-width -1)))))
  (defun hideshow-folded-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... #%d " nlines)))
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  (setq hs-set-up-overlay 'hideshow-folded-overlay-fn)
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"
                 "<!--"
                 sgml-skip-tag-forward
                 nil))
  :bind-keymap
  ("C-c r"           . hs-minor-mode-map)
  :bind
  (:map hs-minor-mode-map
        ("C-c r t"     . hs-toggle-hiding)
        ("C-c r C-M-h" . hs-hide-all)
        ("C-c r C-h"   . hs-hide-block)
        ("C-c r C-M-s" . hs-show-all)
        ("C-c r C-s"   . hs-show-block)
        ("C-c r l"     . hs-hide-level)
        ("C-c r C-a"   . hs-show-all)))

;; expand-region (Melpa)
;; Expand selected region by semantic units.
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C--" . er/contract-region)))

;; treesit.el (Built-in)
(use-package treesit
  :ensure nil
  :config
  ;; treesit-langs (GitHub)
  (use-package treesit-langs
    :ensure (:host github :repo "emacs-tree-sitter/treesit-langs")
    :config
    (treesit-langs-major-mode-setup))

  ;; Code folding
  ;; treesit-fold (GitHub)
  (use-package treesit-fold
    :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
    :config
    (treesit-fold-mode 1)
    ;; (global-treesit-fold-mode)
    :bind
    (("C-c t t"     . treesit-fold-toggle)
     ("C-c t n"     . treesit-fold-open)
     ("C-c t N"     . treesit-fold-close)
     ("C-c t r"     . treesit-fold-open-recursively)
     ("C-c t m"     . treesit-fold-close-all)
     ("C-c t M"     . treesit-fold-open-all))))

;; colorful-mode (Melpa)
;; Rainbow hex color
(use-package colorful-mode
  :hook
  (prog-mode . colorful-mode)
  :config
  (setq colorful-use-prefix t))

;; Parens Settings
;; smartparens (Melpa)
(use-package smartparens
  :hook
  (elpaca-after-init . smartparens-global-mode)
  :config
  (which-key-add-key-based-replacements "C-c s" "Smart Paren")
  (defmacro def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ('(paren . \"(\")
              '(bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
    `(progn
       ,@(cl-loop for '(key . val) in pairs
                  collect
                  `(defun ,(read (concat
                                  "my/sp-wrap-with-"
                                  (prin1-to-string key)
                                  "s"))
                       (&optional arg)
                     (interactive "p")
                     (sp-wrap-with-pair ,val)))))

  (def-pairs ('(paren                . "(")
              '(bracket              . "[")
              '(brace                . "{")
              '(single-quote         . "'")
              '(double-quote         . "\"")
              '(back-quote           . "`")))

  :bind-keymap
  ("C-c s"           . smartparens-mode-map)
  :bind
  (:map smartparens-mode-map
        ("C-c ("   . my/sp-wrap-with-parens)
        ("C-c ["   . my/sp-wrap-with-brackets)
        ("C-c {"   . my/sp-wrap-with-braces)
        ("C-c '"   . my/sp-wrap-with-single-quotes)
        ("C-c \""  . my/sp-wrap-with-double-quotes)
        ("C-c `"   . my/sp-wrap-with-back-quotes)

        ("M-["     . sp-backward-unwrap-sexp)
        ("M-]"     . sp-unwrap-sexp)
        ("C-c s r" . sp-rewrap-sexp)

        ("C-("     . sp-backward-slurp-sexp)
        ("C-{"     . sp-backward-barf-sexp)
        ("C-)"     . sp-forward-slurp-sexp)
        ("C-}"     . sp-forward-barf-sexp)

        ("C-M-a"   . sp-beginning-of-sexp)
        ("C-M-e"   . sp-end-of-sexp)
        ("C-M-n"   . sp-next-sexp)
        ("C-M-p"   . sp-previous-sexp)
        ("C-M-f"   . sp-forward-sexp)
        ("C-M-b"   . sp-backward-sexp)
        ("C-S-f"   . sp-forward-symbol)
        ("C-S-b"   . sp-backward-symbol)

        ("C-M-t"   . sp-transpose-sexp)
        ("C-M-k"   . sp-kill-sexp)
        ("C-k"     . sp-kill-hybrid-sexp)
        ("M-k"     . sp-backward-kill-sexp)
        ("C-M-w"   . sp-copy-sexp)

        ("C-M-u"   . sp-up-sexp)
        ("C-M-d"   . sp-backward-down-sexp)
        ("C-M-S-u" . sp-backward-up-sexp)
        ("C-M-S-d" . sp-down-sexp)
        ("C-M-S-a" . sp-beginning-of-previous-sexp)
        ("C-M-S-e" . sp-end-of-next-sexp)
        ("C-M-S-n" . sp-beginning-of-next-sexp)
        ("C-M-S-p" . sp-beginning-of-previous-sexp)))

;; Rainbow parenthesis (Melpa)
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-depth-1-face  nil  :foreground "#7ffe00" :bold "t")
  (set-face-attribute 'rainbow-delimiters-depth-2-face  nil  :foreground "#1e90ff" :bold "t")
  (set-face-attribute 'rainbow-delimiters-depth-3-face  nil  :foreground "#f5a80f" :bold "t")
  (set-face-attribute 'rainbow-delimiters-depth-4-face  nil  :foreground "#ff1493" :bold "t")
  (set-face-attribute 'rainbow-delimiters-depth-5-face  nil  :foreground "#ba55d3" :bold "t")
  (set-face-attribute 'rainbow-delimiters-depth-6-face  nil  :foreground "#40e0d0" :bold "t")
  (set-face-attribute 'rainbow-delimiters-depth-7-face  nil  :foreground "#32cd32" :bold "t")
  (set-face-attribute 'rainbow-delimiters-depth-8-face  nil  :foreground "#ffd700" :bold "t")
  (set-face-attribute 'rainbow-delimiters-depth-9-face  nil  :foreground "#00ffff" :bold "t"))

;; Highlight current line (Built-in)
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode)
  :config
  (setq hl-line-sticky-flag t)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
                                                (line-beginning-position 2)))))

;; Highlight Thing (Melpa)
(use-package highlight-thing
  :hook
  (prog-mode . highlight-thing-mode)
  :config
  (setq highlight-thing-delay-seconds 0
        highlight-thing-excluded-major-modes
        '(org-mode
          markdown-mode
          help-mode
          eshell-mode
          shell-mode
          term-mode
          vterm-mode
          dired-mode
          magit-mode
          magit-diff-mode)))

;; For ivy-occur
;; wgerp (Melpa)
(use-package wgrep)

;; multiple-cursors (Melpa)
;; Swiper integration.
(use-package multiple-cursors)

;; editorconfig (Melpa)
(use-package editorconfig
  :config
  (editorconfig-mode))

;; emacs-rime (GitHub)
;; Native Rime input method engine in Emacs
(use-package rime
  :ensure (:host github :repo "DogLooksGood/emacs-rime"
           :files ("*.el" "Makefile" "lib.c"))
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-posframe-style 'horizontal)
  ;; Share schemas with fcitx5-rime
  (rime-share-data-dir "/usr/share/rime-data")
  ;; Separate user data dir (yaml configs shared via symlinks)
  (rime-user-data-dir "~/.config/emacs/var/rime")
  (rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g"
                                "<left>" "<right>" "<up>" "<down>"
                                "<prior>" "<next>" "<delete>"))
  (rime-disable-predicates '(rime-predicate-after-alphabet-char-p
                             rime-predicate-prog-in-code-p
                             rime-predicate-ace-window-p
                             rime-predicate-hydra-p
                             rime-predicate-current-uppercase-letter-p))
  :bind
  ("C-`" . rime-force-enable)
  :config
  ;; Preload Rime dynamic module at startup for instant switching
  (activate-input-method "rime")
  (deactivate-input-method)
  ;; Translate C-h/C-w to <backspace>/<escape> during Rime composition so they
  ;; operate on the preedit instead of the buffer. Outside composition, they
  ;; pass through unchanged to the user's normal bindings.
  (define-key key-translation-map (kbd "C-h")
    (lambda (_prompt)
      (if (bound-and-true-p rime-active-mode)
          (kbd "<backspace>")
        (kbd "C-h"))))
  (define-key key-translation-map (kbd "C-w")
    (lambda (_prompt)
      (if (bound-and-true-p rime-active-mode)
          (kbd "<escape>")
        (kbd "C-w"))))
  ;; C-g clears composition (same as <escape>) during Rime input
  (define-key rime-active-mode-map (kbd "C-g") 'rime--escape)
  ;; Finalize Rime before Emacs exits to prevent librime atexit segfault
  (add-hook 'kill-emacs-hook
            (lambda ()
              (when (and (fboundp 'rime-lib-finalize) rime--lib-loaded)
                (ignore-errors (rime-lib-finalize))))))

;; sis (Melpa)
;; Smart Input Source to minimize manual switching input source in Emacs.
(use-package sis
  :after rime
  :config
  (sis-ism-lazyman-config nil "rime" 'native)
  (sis-global-cursor-color-mode t)
  (sis-global-respect-mode t)
  (sis-global-context-mode t)
  (sis-global-inline-mode t))

;; cns (GitHub)
;; Chinese word segmentation for M-f/M-b/M-d etc.
;; Requires: C++ compiler and make (auto-compiled via :pre-build)
(use-package cns
  :ensure (:host github :repo "kanglmf/emacs-chinese-word-segmentation"
           :files ("cns.el")
           :pre-build (("git" "submodule" "update" "--init" "--recursive")
                       ("make")))
  :custom
  (cns-process-type 'shell)
  (cns-prog (expand-file-name
             "elpaca/repos/emacs-chinese-word-segmentation/cnws"
             user-emacs-directory))
  (cns-dict-directory (expand-file-name
                       "elpaca/repos/emacs-chinese-word-segmentation/cppjieba/dict"
                       user-emacs-directory))
  (cns-recent-segmentation-limit 20)
  (cns-debug nil)
  :hook (find-file . cns-auto-enable)
  :config
  ;; Make backward-kill-word respect Chinese word segmentation when cns-mode is active.
  ;; This allows C-w (bound to backward-kill-word via general) to work with Chinese words.
  (define-advice backward-kill-word (:around (orig-fn arg) cns-aware)
    (if (bound-and-true-p cns-mode)
        (cns-backward-kill-word arg)
      (funcall orig-fn arg))))


(provide 'init-editing)

;;; init-editing.el ends here
