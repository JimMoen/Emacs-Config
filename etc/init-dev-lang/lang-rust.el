;;; lang-rust.el --- Rust support. -*- lexical-binding: t -*-

;; Copyright (C) 2024  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: Rust


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

;; Programming Language Rust support.

;;; Code:

;; rust-mode (Melpa)
(use-package rust-mode
  :init
  (add-to-list 'auto-mode-alist '("\\cargo.lock\\'" . toml-ts-mode))
  :config
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode)))

;; flycheck-rest (Melpa)
(use-package flycheck-rust
  :after (rust-mode flychcek)
  :hook (rust-mode . flycheck-rust-setup))

;; cargo-mode (Melpa)
(use-package cargo-mode)


(provide 'lang-rust)

;;; lang-rust.el ends here
