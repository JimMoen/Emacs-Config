;;; ab-base.el --- Personal Basic Feature -*- lexical-binding: t -*-

;; Copyright (C) 2021  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: ivy counsel swiper


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

;; Personal Basic Feature Settings.

;;; Code:

;; general
(use-package general)

;; ivy & counsel & swiper
;; ivy (Melpa)
(use-package ivy
  :hook
  (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers      t
        enable-recursive-minibuffers nil)
  :bind
  ;; Use persp-mode to switch/kill buffer in ONE project.
  ;; See ./etc/ab-base.el: persp-mode
  (("C-x B"    . ivy-switch-buffer)
   ("C-c C-o"  . ivy-occur)
   ("C-c C-r"  . ivy-resume)
   ("C-c v"    . ivy-push-view)
   ("C-c V"    . ivy-pop-view)))

;; counsel (Melpa)
(use-package counsel
  :ensure-system-package
  (rg          . ripgrep)
  ;; "rg" to use counsel-rg
  :config
  (setq counsel-rg-base-command
        (list "rg" "-M" "240" "--with-filename" "--no-heading" "--line-number" "--color" "never" "%s"
              "-g" "!site-lisp" "-g" "!elpa" "-g" "!var"  ;; ignore site-lisp/ elpa/ var/ in user-emacs-directory
              ))
  :bind
  (("M-x"       . counsel-M-x)
   ("C-x C-f"   . counsel-find-file)
   ("C-x C-r"   . counsel-recentf)
   ("C-c k"     . counsel-rg)
   ("C-x 8 RET" . counsel-unicode-char)
   ("<f1> j"    . counsel-set-variable)
   ("C-c g"     . counsel-git)
   ("C-c j"     . counsel-git-grep))
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history)))

;; swiper (Melpa)
(use-package swiper
  :bind
  (("C-s"       . swiper)
   ("C-r"       . swiper)))


;; Files Management & Auto Save
;; Dired (Built-in)
(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-Afhlv"))

;; Dired-Single (Melpa)
(use-package dired-single
  :config
  (setq dired-single-magic-buffer-name "*Dired*")
  :bind
  ((("C-x d"     . 'dired-single-magic-buffer))
   (:map dired-mode-map
         (("RET" . dired-single-buffer)
          ("e"   . dired-single-buffer)
          ("f"   . dired-single-buffer)
          ("^"   . dired-single-up-directory)
          ("E"   . dired-single-toggle-buffer-name)
          ("M-g" . dired-goto-file)))))

;; autorevert (Built-in)
(use-package autorevert
  :ensure nil
  :hook
  (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;; Recentf (Built-in)
(use-package recentf
  :ensure nil
  :custom
  (make-backup-files nil)
  ;; never make backup files like "init.el~"
  :init
  (setq recentf-max-saved-items 100
        recentf-exclude         '("/tmp/" "/ssh:"))
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory))

;; bookmark (Built-in)
(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-save-flag     t))


;; The Keybindings and Key Hint
;; #### Personal Settings ####
;; I use C-h to delete a char and C-w to delete a word just like in terminal.
;; Use 'C-M-g'   avy-goto-char-in-line
;; Use 'M-g M-g' avy-goto-char-2
;; See ./etc/ad-editing.el: avy
(use-package emacs
  :ensure nil
  :bind
  (("C-h"     . backward-delete-char-untabify)
   ("C-w"     . backward-kill-word)
   ("C-c M-w" . kill-region)
   ("C-x h"   . help-command)
   ("C-x w h" . mark-whole-buffer)
   ("M-g g"   . nil)
   ("M-g M-g" . nil)))

;; which-key (Melpa)
(use-package which-key
  :hook
  (after-init . which-key-mode)
  :init
  (which-key-setup-side-window-bottom)
  :config
  (setq which-key-idle-delay               2.0
        which-key-max-description-length   30
        which-key-add-column-padding       8
        which-key-max-display-columns      5
        which-key-show-remaining-keys      t
        which-key-separator                " -> "
        which-key-prefix-prefix            " ++ "
        which-key-special-keys             '("SPC" "TAB" "RET" "ESC" "DEL")))



;; Buffer Management
;; ibuffer (Built-in)
(use-package ibuffer
  :ensure nil
  :preface
  (defun ibuffer-switch-to-normal ()
    "ibuffer swith to normal filter groups."
    (interactive)
    (ibuffer-switch-to-saved-filter-groups "Normal"))
  :hook
  ((ibuffer-mode . ibuffer-switch-to-normal)
   (ibuffer-mode . ibuffer-auto-mode))
  :config
  (setq ibuffer-saved-filter-groups
        '(("Normal"
           ("Emacs"      (or
                          (name . "^\\*scratch\\*$"   )
                          (name . "^\\*Messages\\*$"  )
                          (name . "^\\*Backtrace\\*$" )
                          (name . "^\\*Packages\\*")
                          (name . "^\\*Calendar\\*")
                          (mode . dashboard-mode)))
           ("Info Out"   (or
                          (name . "^\\*compilation\\*$")
                          (name . "\\*Compile-Log\\*")
                          (mode . comint)))
           ("Dired"      (or
                          (mode . dired-mode)))
           ("Magit-main" (or
                          (name . "^magit:")))
           ("Magit-etc"  (or
                          (name . "^magit-")))
           ("Text"       (or
                          (mode . org-mode)
                          (mode . markdown)
                          (mode . text-mode)))
           ("Config"     (or
                          (mode . yaml-mode)
                          (mode . conf-mode)))
           ("Program"    (or
                          (mode . emacs-lisp-mode)
                          (mode . python-mode)
                          (mode . html-mode)
                          (mode . javascript-mode)
                          (mode . css-mode)
                          (mode . java-mode)
                          (mode . haskell-mode)
                          (mode . lisp-mode)
                          (mode . erlang-mode)
                          (mode . prog-mode)))
           ("GNUs"       (or
                          (mode . message-mode)
                          (mode . bbdb-mode)
                          (mode . gnus-group-mode)
                          (mode . gnus-summary-mode)
                          (mode . gnus-article-mode)
                          (name . "^\\.bbdb$")
                          (name . "^\\.newsrc-dribble")))
           ("Help"       (or
                          (name . "^\\*Help\\*$")
                          (name . "^\\*Apropos\\*$")
                          (name . "^\\*info\\*$")
                          (name . "^\\*helpful")
                          (name . "^\\*Disabled Command\\*$")))
           ("Youdao"     (or
                          (name . "^\\*Youdao Dictionary\\*$")
                          (mode . youdao-dictionary-mode)))
           ("Custom"     (or
                          (name . "^\\*Customize")
                          (mode . custom-mode))))))
  (setq ibuffer-show-empty-filter-groups   nil
        ibuffer-default-sorting-mode       'filename/process)
  :bind
  ;; This command is for viewing all buffers.
  ;; Viewing specify buffers in current project by "projectile-ibuffer"
  ;; See ./etc/ba-dev-tools.el: projectile
  (("C-x C-S-b"     . ibuffer)))



;; switch-window (Melpa)
(use-package switch-window
  :config
  (setq switch-window-threshold 3)
  (which-key-add-key-based-replacements "C-x 4" "Switch Window")
  :general
  ("C-\'" 'switch-window)
  (:prefix "C-x"
           "0"    'switch-window-then-delete
           "1"    'switch-window-then-maximize
           "2"    'switch-window-then-split-below
           "3"    'switch-window-then-split-right
           "4v"   'switch-window-then-split-vertically
           "4h"   'switch-window-then-split-horizontally
           "4s"   'switch-window-then-swap-buffer
           "4d"   'switch-window-then-dired
           "4f"   'switch-window-then-find-file
           "4R"   'switch-window-then-find-file-read-only
           "4b"   'switch-window-then-display-buffer
           "40"   'switch-window-then-kill-buffer)
  (:keymaps 'switch-window-extra-map
            "i"   'nil
            "p"   'switch-window-mvborder-up
            "k"   'nil
            "n"   'switch-window-mvborder-down
            "j"   'nil
            "b"   'switch-window-mvborder-left
            "l"   'nil
            "f"   'switch-window-mvborder-right
            "a"   'balance-windows
            "SPC" 'switch-window-resume-auto-resize-window))

;; shackle (Melpa)
(use-package shackle
  :hook
  (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.35)
  (shackle-default-alignment 'below)
  (shackle-rules '((magit-status-mode          :select t   :inhibit-window-quit nil :same t)
                   (magit-log-mode             :select t   :inhibit-window-quit nil :same t)
                   (help-mode                  :select t   :inhibit-window-quit nil :align right :size 0.40)
                   (comint-mode                :select t   :align t   :size 0.40)
                   (grep-mode                  :select t   :align t)
                   (rg-mode                    :select t   :align t)
                   (ivy-occur-grep-mode        :select t   :align right :size 0.40)
                   ("*diff-hl*"                :select nil :align below :size 0.40)
                   ("*vc-diff*"                :select t   :align below :size 0.40)
                   ("*bm-bookmarks*"           :select t   :align t)
                   ("*Backtrace*"              :select t   :align t   :size 15)
                   ("*package update results*" :select nil :align t   :size 10)
                   ("*Process List*"           :select t   :align t   :size 0.30))))
;;TODO

;; winner (Built-in)
(use-package winner
  :ensure nil
  :hook
  (after-init . winner-mode)
  :commands (winner-undo winner-redo)
  :bind
  (("C-<"        . winner-undo)
   ("C->"        . winner-redo))
  :config
  (setq winner-boring-buffers '("*Backtrace*"
                                "*Completions*"
                                "*Compile-Log*")))


(provide 'ab-base)

;;; ab-base.el ends here
