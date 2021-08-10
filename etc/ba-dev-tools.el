;;; ba-dev-tools.el --- Development Tools. -*- lexical-binding: t -*-

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

;; VCS-Git
;; Magit (Melpa)
(use-package magit
  :config
  (setq magit-status-margin            '(t age-abbreviated   magit-log-margin-width t 18)
        magit-refs-margin              '(t age-abbreviated   magit-log-margin-width t 18)
        magit-reflog-margin            '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)
        magit-log-section-commit-count 50)
  (setq magit-status-initial-section
        '(((unpulled . "..@{upstream}") (status))
          ((untracked) (status))
          ((unstaged) (status))
          1))
  (set-face-attribute 'magit-diff-file-heading nil :foreground "#f57bae" :weight 'bold)
  :bind
  (("C-x g" . magit-status)))

;; Project Management
;; projectile (Melpa)
(use-package projectile
  :hook
  (prog-mode . projectile-mode)
  :config
  (setq projectile-completion-system 'ivy)
  :bind
  (("C-x C-b" . projectile-ibuffer))
  (:map projectile-command-map
        ("r"     . nil))
  :bind-keymap
  ("C-x p" . projectile-command-map))

;; counsel-projectile (Melpa)
(use-package counsel-projectile
  :after counsel projectile
  :hook
  (after-init . counsel-projectile-mode))

;; persp-mode to managment projcet buffers (Melpa)
(require-all-elisp-in-directory "etc/editor-layouts")

;; Code Check
;; flycheck (Melpa)
(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit))

;; Code template
;; yasnippet (Melpa)
(use-package yasnippet)

;; yasnippet-snippets (Melpa)
(use-package yasnippet-snippets)


(provide 'ba-dev-tools)

;;; ba-dev-tools.el ends here
