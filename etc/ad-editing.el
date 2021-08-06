;;; ad-editing.el --- Easy Editing. -*- lexical-binding: t -*-

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
  (fill-column               80)                        ;; fill column sets to 80
  (indent-tabs-mode          nil)                       ;; Use only spaces and no tabs
  (tab-width                 4)                         ;; Tab width set to 4
  (standard-indent           4)                         ;; Default indent sets 4
  :config
  (blink-cursor-mode -1)
  :bind
  (("RET"                    .  newline-and-indent)
   ("S-<return>"             .  comment-indent-new-line)))

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
  (display-line-numbers-width 3))

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
  (set-face-attribute 'whitespace-trailing nil :foreground "#79cdcd" :background "#668b8b")
  :bind
  (("C-c t c" . whitespace-cleanup)
   ("C-c t C" . whitespace-cleanup-region)
   ("C-c t r" . whitespace-report)
   ("C-c t R" . whitespace-report-region)))

;; avy to jump char (Melpa)
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
   ("M-g M-c" . avy-goto-char)
   ("M-g M-g" . avy-goto-char-2)
   ("M-g s"   . avy-goto-whitespace-end)
   ("M-g M-a" . avy-goto-line)
   ("M-g M-e" . avy-goto-end-of-line)
   ("M-g w"   . avy-goto-word-1)))


(provide 'ad-editing)

;;; ad-editing.el ends here
