;;; lang-hocon.el --- Hocon & hcl support. -*- lexical-binding: t -*-

;; Copyright (C) 2021  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: Hocon, hcl


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

;; Language Hocon support.

;;; Code:

;; hcl-mode (Melpa)
;; for hocon and hcl
(use-package hcl-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.hocon$" . hcl-mode)))


(provide 'lang-hocon)

;;; lang-hocon.el ends here
