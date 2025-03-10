;;; lang-erlang.el --- Erlang support. -*- lexical-binding: t -*-

;; Copyright (C) 2021  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: Erlang


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

;; Programming Language Erlang support.

;;; Code:

(use-package erlang-ts
  :general
  (:keymaps 'erlang-ts-mode-map
            "C-c C-l" nil)
  (:keymaps 'erlang-mode-map
            "C-c C-l" nil)
  :config
  (add-to-list 'treesit-language-source-alist '(erlang "https://github.com/WhatsApp/tree-sitter-erlang"))
  (setq lsp-erlang-server 'erlang-language-platform)
  (defun my/modified-syntax-table ()
    (put 'bitsyntax-open-outer 'syntax-table nil)
    (put 'bitsyntax-close-outer 'syntax-table nil))
  (setq erlang-indent-level      4
        erlang-icr-indent        4
        erlang-indent-guard      0
        erlang-argument-indent   4
        erlang-tab-always-indent t)

  (with-eval-after-load 'apheleia
    (setf (alist-get 'erlang-ts-mode apheleia-mode-alist)
          '(erlfmt))
    (setf (alist-get 'erlfmt apheleia-formatters)
          '("erlfmt" "-")))

  (with-eval-after-load 'smartparens
    (sp-with-modes '(erlang-ts-mode erlang-mode)
      (sp-local-pair "<<" ">>")
      (sp-local-pair "#{" "}"))
    (def-pairs ('(double-angle-bracket . "<<")))
    (define-key smartparens-mode-map (kbd "C-c ,") 'my/sp-wrap-with-double-angle-brackets))

  (advice-add #'erlang-electric-init :after 'my/modified-syntax-table))

(provide 'lang-erlang)

;;; lang-erlang.el ends here
