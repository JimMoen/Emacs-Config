;;; lang-c3.el --- C3 support. -*- lexical-binding: t -*-

;; Copyright (C) 2025  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: C3


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

;; Programming Language C3 support.

;;; Code:

(use-package c3-ts-mode
  :straight (:host github :repo "c3lang/c3-ts-mode" :files ("*.el"))
  :config
  (add-to-list 'treesit-language-source-alist '(c3 "https://github.com/c3lang/tree-sitter-c3")))

(provide 'lang-c3)

;;; lang-c3.el ends here
