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


(provide 'ad-editing)

;;; ad-editing.el ends here
