;;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;;; File: ae-utils.el
;;;; Auth: JimMoen
;;;; Code:

;;;; ####################################################### Chinese input method
;;;;                                              ========== rime (None Built-in)
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



;;;; ####################################################### Calendar Settings
;;;;                                              ========== calendar (Built-in)
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


(use-package cal-china-x
  :config
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays)))



;;;; ####################################################### English Translate
;;;;                                              ========== youdao-dictionary (None Bulit-in)
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

;;;;                                              ========== english-translate (None Bulit-in)
(use-package english-teacher
  :load-path "site-lisp/english-teacher"
  :custom
  (english-teacher-backend 'baidu)
  (english-teacher-show-result-function 'english-teacher-default-show-result-function)
  :hook
  (('Info-mode        . 'english-teacher-follow-mode)
   ('elfeed-show-mode . 'english-teacher-follow-mode)
   ('eww-mode         . 'english-teacher-follow-mode)
   ('Man-mode         . 'english-teacher-follow-mode)
   ('Woman-mode       . 'english-teacher-follow-mode))
  :bind
  (("C-x y f" . english-teacher-follow-mode)))



;;;; ####################################################### Other Useful Applications
;;;;                                              ========== speed-type (None Built-in)
(use-package speed-type
  :defer t)

;;;;                                              ========== pdf-tools (None Built-in)
(use-package pdf-tools
  :defer t)

;;;;                                              ========== info-colors (None Built-in)
(use-package info-colors
  :defer t
  :hook
  ('Info-selection . 'info-colors-fontify-node))



;;;; ####################################################### ;;;;
(provide 'ae-utils)
;;;; ae-utils.el ends here
