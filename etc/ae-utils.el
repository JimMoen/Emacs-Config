;;; ae-utils.el --- Useful Apps -*- lexical-binding: t -*-

;; Copyright (C) 2021  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: Utility Apps


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

;; Chinese Input Method.  Translate Apps
;; Calendar Settings.
;; And some other tools.

;;; Code:

;; Chinese input method
;; rime (Melpa)
(use-package rime
  :custom
  (default-input-method "rime")
  :config
  (setq rime-translate-keybindings
        '("C-h" "C-g" "C-d" "C-k"
          "C-f" "C-b" "C-n" "C-p" "C-a" "C-e" "C-v" "M-v" "<tab>" "S-<tab>" "C-`"
          "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))
  (setq rime-user-data-dir "~/.local/share/fcitx5/rime")  ;; Use Fcitx5
  (setq rime-show-candidate 'posframe)
  (setq rime-posframe-properties
        (list :background-color "#555555"
              :foreground-color "#dcdcdc"
              :font "sarasa mono sc-13"
              :internal-border-width 4)))

;; English Translate
;; youdao-dictionary (Melpa)
(use-package youdao-dictionary
  :defer nil
  :config
  (setq url-automatic-caching t)
  (which-key-add-key-based-replacements "C-x y" "Youdao Dic")
  (defun youdao-search-and-play-voice-at-point ()
    (interactive)
    (youdao-dictionary-play-voice-at-point)
    (youdao-dictionary-search-at-point-tooltip))
  :bind
  (("C-x y t" . 'youdao-search-and-play-voice-at-point)
   ("C-x y v" . 'youdao-dictionary-play-voice-at-point)
   ("C-x y r" . 'youdao-dictionary-search-and-replace)
   ("C-x y i" . 'youdao-dictionary-search-from-input)))


(provide 'ae-utils)

;;; ae-utils.el ends here
