;;; lang-python.el --- Python support. -*- lexical-binding: t -*-

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

;; python (Built-in)
(use-package python
  :ensure nil
  :hook
  (python-mode . (lambda ()
                   (setq-local indent-tabs-mode nil)
                   (setq-local tab-width 4)
                   (setq-local python-indent-offset 4))))

;; lsp-pyright (Melpa)
(use-package lsp-pyright)

;; uv-mode (Melpa)
(use-package uv-mode
  :hook (python-mode . uv-mode-auto-activate-hook))

(provide 'lang-python)

;;; lang-python.el ends here
