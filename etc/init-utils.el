;;; init-utils.el --- Useful Apps -*- lexical-binding: t -*-

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

;; English Translate
;; youdao-dictionary (Melpa)
(use-package youdao-dictionary
  :defer nil
  :config
  (use-package pos-tip)
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

;; http(s) and socks proxy
;; use-praxy (Melpa)
(use-package use-proxy
  :config
  (setq use-proxy-https-proxy "127.0.0.1:18889")
  (setq use-proxy-http-proxy  "127.0.0.1:18889"))

;; english-teacher (Site Package)
;; For English sentence translating.
;; https://github.com/loyalpartner/english-teacher.el
(use-package english-teacher
  :load-path "site-elisp/english-teacher"
  :custom
  (english-teacher-backend 'baidu)
  (english-teacher-show-result-function 'english-teacher-default-show-result-function)
  :hook
  ((Info-mode        . english-teacher-follow-mode)
   (elfeed-show-mode . english-teacher-follow-mode)
   (eww-mode         . english-teacher-follow-mode)
   (Man-mode         . english-teacher-follow-mode)
   (Woman-mode       . english-teacher-follow-mode))
  :bind
  (("C-x y f" . english-teacher-follow-mode)))

;; ssh-config-mode (Melpa)
;; major mode for ssh config file
(use-package ssh-config-mode
  :defer t)

;; Calendar Settings
;; calendar (Built-in)
(use-package calendar
  :ensure nil
  :config
  (setq holiday-local-holidays `((holiday-fixed 3 8  "Women's Day")
                                 (holiday-fixed 3 12 "Arbor Day")
                                 ,@(cl-loop for i from 1 to 3
                                            collect `(holiday-fixed 5 ,i "International Workers' Day"))
                                 (holiday-fixed 5 4  "Chinese Youth Day")
                                 (holiday-fixed 6 1  "Children's Day")
                                 (holiday-fixed 9 10 "Teachers' Day")
                                 ,@(cl-loop for i from 1 to 7
                                            collect `(holiday-fixed 10 ,i "National Day"))
                                 (holiday-fixed 10 24 "Programmers' Day")
                                 (holiday-fixed 11 11 "Singles' Day")))
  (setq holiday-other-holidays '((holiday-fixed 4 22 "Earth Day")
                                 (holiday-fixed 4 23 "World Book Day")
                                 (holiday-sexp '(if (or (zerop (% year 400))
                                                        (and (% year 100) (zerop (% year 4))))
                                                    (list 9 12 year)
                                                  (list 9 13 year))
                                               "World Programmers' Day")))
  (setq calendar-chinese-all-holidays-flag t))

;; cal-china-x (Melpa)
;; For China holidays.
(use-package cal-china-x
  :config
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays)))

;; i3wm-config-mode (Melpa)
;; for i3wm confi files
(use-package i3wm-config-mode
  :defer t)

;; systemd (Melpa)
;; systemd unit editing major mode
(use-package systemd
  :defer t)

;; pkgbuild-mode (Melpa)
;; Arch Linux PKGBUILD Scripts major mode
(use-package pkgbuild-mode
  :defer t)

;; Other Useful Applications
;; speed-type (Melpa)
(use-package speed-type
  :defer t)

;; pdf-tools (Melpa)
(use-package pdf-tools
  :defer t)

;; info-colors (Melpa)
;; Add color support for info pages.
(use-package info-colors
  :defer t
  :hook
  (Info-selection . info-colors-fontify-node))

;; restclient (Melpa)
;; rest api client and company backend
(use-package restclient
  :defer t
  :config
  (use-package company-restclient
    :defer t))

;; protobuf-mode (Melpa)
;; Editing Google protocol buffer file
(use-package protobuf-mode
  :defer t)

(use-package direnv
  :init
  (setq direnv-always-show-summary nil)
  :config
  (direnv-mode))

(provide 'init-utils)

;;; init-utils.el ends here
