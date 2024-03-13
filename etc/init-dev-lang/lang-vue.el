;;; lang-vue.el --- VUE support. -*- lexical-binding: t -*-

;; Copyright (C) 2021  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: VUE


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

;; Programming Language VUE support.

;;; Code:

(use-package typescript-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2))

(provide 'lang-vue)

;;; lang-vue.el ends here
