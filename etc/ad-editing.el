;;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;;; File: ad-editing.el
;;;; Auth: JimMoen
;;;; Code:

;;;; ####################################################### Indent Settings
;;;;                                              ========== Emacs (Built-in)
(use-package emacs
  :ensure nil
  :custom
  (fill-column                 80)                        ;; fill column sets to 80
  (indent-tabs-mode            nil)                       ;; Use only spaces and no tabs
  (tab-width                   4)                         ;; Tab width set to 4
  (standard-indent             4)                         ;; Default indent sets 4
  :config
  (blink-cursor-mode -1)
  :bind
  (("RET"                    .  newline-and-indent)
   ("S-<return>"             .  comment-indent-new-line)))

;;;;                                              ========== align (Built-in)
(use-package align
  :ensure nil
  :bind
  (("C-c =" . align-regexp)))

;;;;                                              ========== wgerp (None Built-in)
(use-package wgrep)



;;;; ####################################################### Intuitional editing
;;;;                                              ========== Display Line numbers (Built-in)
(use-package display-line-numbers
  :ensure nil
  :hook
  ('after-init . 'global-display-line-numbers-mode)
  :custom
  (display-line-numbers-width 3))

;;;;                                              ========== Display Column numbers (Built-in)
(use-package simple
  :ensure nil
  :hook
  ('after-init . 'column-number-mode))

;;;;                                              ========== delsel (Built-in)
(use-package delsel
  :ensure nil
  :hook
  ('after-init . 'delete-selection-mode))

;;;;                                              ========== so-long (Built-in)
(use-package so-long
  :ensure nil
  :hook
  ('after-init . 'global-so-long-mode))

;;;;                                              ========== subword (Built-in)
(use-package subword
  :ensure nil
  :hook
  ('after-init . 'global-subword-mode))

;;;;                                              ========== Whitespace Display (Built-in)
(use-package whitespace
  :ensure nil
  :defer t
  :hook
  ('after-init . 'global-whitespace-mode)
  :config
  (setq whitespace-style
        '(face tabs                     tab-mark
               space-after-tab::space   space-before-tab::space
               indentation::space
               trailing                 empty))
  (setq whitespace-display-mappings
      '(;; "tab" char.      Display like "|   ".   Or Display like "\   "
        (tab-mark      9   [124 9]   [92 9])
        ;; " " char.        Display like "·".      Or Display like "_"
        (space-mark    32  [183]     [95])
        ;; "newline" char.  Display like "¬"       Or Display like "¶"
        (newline-mark  10  [172 10]  [182 10])))
  (set-face-attribute 'whitespace-tab      nil :foreground "#444444" :background "#686868")
  (set-face-attribute 'whitespace-empty    nil :foreground "#cd8c95" :background "#8b5f65")
  (set-face-attribute 'whitespace-trailing nil :foreground "#79cdcd" :background "#668b8b"))

;;;;                                              ========== avy to jump char (None Built-in)
(use-package avy
  :config
  (setq avy-timeout-seconds 0.1
        avy-background      nil)
  (set-face-attribute 'avy-lead-face       nil :foreground "#1d1f21" :background "#ef9299")
  (set-face-attribute 'avy-lead-face-0     nil :foreground "#1d1f21" :background "#8898bf")
  (set-face-attribute 'avy-lead-face-1     nil :foreground "#1d1f21" :background "#9ac1c8")
  (set-face-attribute 'avy-lead-face-2     nil :foreground "#1d1f21" :background "#f1d8b3")
  :bind
  (("C-M-g"   . avy-goto-char-in-line)
   ("M-g c"   . avy-goto-char)
   ("M-g M-g" . avy-goto-char-2)
   ("M-g s"   . avy-goto-whitespace-end)
   ("M-g a"   . avy-goto-line)
   ("M-g e"   . avy-goto-end-of-line)
   ("M-g w"   . avy-goto-word-1)))



;;;; ####################################################### For Code Editing
;;;;                                              ========== hideshow (Built-in)
(use-package hideshow
  :ensure nil
  :custom
  (hs-hide-comments-when-hiding-all nil)
  :hook
  ('prog-mode . 'hs-minor-mode)
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :weight semi-bold :box (:line-width -1)))))
  (defun hideshow-folded-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... #%d " nlines)))
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  (setq hs-set-up-overlay 'hideshow-folded-overlay-fn)
  :bind
  (:map hs-minor-mode-map
        ("C-c t t"     . hs-toggle-hiding)
        ("C-c t C-M-h" . hs-hide-all)
        ("C-c t C-h"   . hs-hide-block)
        ("C-c t C-M-s" . hs-show-all)
        ("C-c t C-s"   . hs-show-block)
        ("C-c t l"     . hs-hide-level)
        ("C-c @ C-a"   . hs-show-all)))

;;;;                                              ========== Rainbow hex color
(use-package rainbow-mode
  :hook
  ('prog-mode . 'rainbow-mode))



;;;; ####################################################### Parens Settings
;;;;                                              ========== smartparens (None Built-in)
(use-package smartparens
  :hook
  ('after-init       . 'smartparens-global-mode)
  :config
  (which-key-add-key-based-replacements "C-c s" "Smart Paren")
  :bind-keymap
  ("C-c s"           . smartparens-mode-map)
  :bind
  (:map smartparens-mode-map
        ("C-c s U"   . sp-unwrap-sexp)
        ("C-c s R"   . sp-rewrap-sexp)
        ("C-c C-u"   . sp-backward-up-sexp)
        ("C-c C-d"   . sp-down-sexp)
        ("C-c C-a"   . sp-beginning-of-sexp)
        ("C-c C-S-a" . sp-beginning-of-previous-sexp)
        ("C-c C-e"   . sp-end-of-sexp)
        ("C-c C-S-e" . sp-end-of-next-sexp)
        ("C-c C-n"   . sp-beginning-of-next-sexp)
        ("C-c C-p"   . sp-beginning-of-previous-sexp)))

;;;;                                              ========== Rainbow parenthesis (None Built-in)
(use-package rainbow-delimiters
  :hook
  ('prog-mode . 'rainbow-delimiters-mode)
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



;;;; ####################################################### ;;;;
(provide 'ad-editing)
;;;; ad-editing.el ends here
