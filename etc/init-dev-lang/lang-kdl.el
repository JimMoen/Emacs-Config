;;; lang-kdl.el --- KDL support. -*- lexical-binding: t -*-

;; Copyright (C) 2026  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: KDL

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

;; Language KDL support.

;;; Code:

;; kdl-mode (Melpa)
;; Requires tree-sitter-kdl grammar for syntax highlighting.
;; Run M-x kdl-install-tree-sitter-grammar if not auto-installed.
(use-package kdl-mode
  :ensure (:host github :repo "taquangtrung/emacs-kdl-mode")
  :mode "\\.kdl\\'"
  :config
  (add-to-list 'my/disabled-lsp-major-modes 'kdl-mode))


(provide 'lang-kdl)

;;; lang-kdl.el ends here
