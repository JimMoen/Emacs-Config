;;; init-base.el --- Personal Basic Feature -*- lexical-binding: t -*-

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

;; The Keybindings and Key Hint
;; #### Personal Settings ####
;; I use C-h to delete a char and C-w to delete a word just like in terminal.
;; Use 'C-M-g'   avy-goto-char-in-line
;; Use 'M-g M-g' avy-goto-char-2
;; See ./etc/init-editing.el: avy
(use-package emacs
  :ensure nil
  :config
  (setq ad-redefinition-action 'accept)
  :general
  ("C-h"     'backward-delete-char-untabify
   "C-w"     'backward-kill-word
   "M-w"     'kill-ring-save
   "M-W"     'kill-region
   "C-x h"   'help-command
   "C-x H"   'mark-whole-buffer
   "M-g g"   'nil
   "M-g M-g" 'nil
   "<f12>"   'list-packages))

;; nerd-icons (Melpa)
(use-package nerd-icons
  :config
  (defun update-alist (alist-symbol rep-alist)
    "Update the alist specified by ALIST-SYMBOL with entries from REP-ALIST.
If a key from REP-ALIST is present in the alist referred to by ALIST-SYMBOL,
its value will be updated. If the key is not present, the entry will be added."
    (let ((alist (symbol-value alist-symbol)))
      (dolist (rep rep-alist)
        (let ((key (car rep))
              (value (cdr rep)))
          (if (assoc key alist)
              (setcdr (assoc key alist) value)
            (setq alist (cons rep alist)))))
      (set alist-symbol alist)))

  (update-alist 'nerd-icons-extension-icon-alist
                '(
                  ("ini"        nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  ("properties" nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)

                  ("json"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  ("jsonl"      nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  ("cson"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  ("yml"        nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dyellow)
                  ("yaml"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dyellow)
                  ("toml"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-orange)
                  ("conf"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dorange)

                  ("tscn"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-orange)
                  ("tres"       nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-orange)

                  ("config"     nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dyellow)
                  ))
  (update-alist 'nerd-icons-mode-icon-alist
                '(
                  (Custom-mode  nerd-icons-sucicon "nf-seti-settings")

                  (conf-mode    nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-lyellow)
                  (json-mode    nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  (json-ts-mode nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  (jsonian-mode nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-yellow)
                  (yaml-mode    nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dyellow)
                  (yaml-ts-mode nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-dyellow)
                  (toml-mode    nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-orange)
                  (toml-ts-mode nerd-icons-sucicon "nf-seti-settings" :face nerd-icons-orange)
                  ))
  (update-alist
   'nerd-icons-regexp-icon-alist
   '(
     ("^rebar3.crashdump$"          nerd-icons-devicon "nf-dev-erlang"        :face nerd-icons-lred)
     ("^rebar.lock$"                nerd-icons-devicon "nf-dev-erlang"        :face nerd-icons-red)
     ("^rebar.config$"              nerd-icons-devicon "nf-dev-erlang"        :face nerd-icons-green)
     ("^security"                   nerd-icons-faicon  "nf-fa-lock"           :face nerd-icons-lcyan)
     ("^rebar3$"                    nerd-icons-devicon "nf-dev-erlang"        :face nerd-icons-orange)
     ("^PKGBUILD$"                  nerd-icons-flicon  "nf-linux-archcraft"   :face nerd-icons-lblue)
     ("^\\.?gitignore"              nerd-icons-sucicon "nf-seti-git_ignore"   :face nerd-icons-lred)
     ("^\\.?git-blame-ignore-revs$" nerd-icons-sucicon "nf-seti-git"          :face nerd-icons-lcyan)
     ("^\\.editorconfig$"           nerd-icons-sucicon "nf-seti-editorconfig" :face nerd-icons-silver)
     ))
  (setq nerd-icons-font-family "Sarasa Gothic SC Nerd Font"
        nerd-icons-scale-factor 0.8))

;; ivy & counsel & swiper
;; ivy (Melpa)
(use-package ivy
  :hook
  (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers      t
        enable-recursive-minibuffers nil
        ivy-height                   15)

  (use-package nerd-icons-ivy-rich
    :ensure t
    :after (counsel-projectile)
    :init
    (nerd-icons-ivy-rich-mode 1)
    (ivy-rich-mode 1)
    :config
    (setq nerd-icons-ivy-rich-icon-size 1.0))

  (use-package ivy-rich
    :after (nerd-icons-ivy-rich))

  (use-package ivy-hydra
    :after (ivy hydra))

  :bind
  ;; Use persp-mode to switch/kill buffer in ONE project.
  ;; See ./etc/init-base.el: persp-mode
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
              "-g" "!.git" ;; ignore .git directory
              "-g" "!site-lisp" "-g" "!elpa" "-g" "!var" "-g" "!straight"
              ;; ignore site-lisp/ elpa/ var/ in user-emacs-directory
              "--case-sensitive" "--hidden" "--multiline"
              ;; search hidden directories
              ))
  (use-package emacs
    :after (ivy-rich)
    :general
    (:prefix "C-x h"
             "v" 'counsel-describe-variable
             "f" 'counsel-describe-function
             "o" 'counsel-describe-symbol))

  ;; counsel-projectile (Melpa)
  (use-package counsel-projectile
    :after (ivy counsel projectile)
    :hook
    (after-init . counsel-projectile-mode))

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

;; counsel-tramp (Melpa)
(use-package counsel-tramp
  :config
  (setq tramp-default-method "sshx")
  :general
  ("C-c S" 'counsel-tramp))

;; swiper (Melpa)
(use-package swiper
  :bind
  (("C-s"       . swiper)
   ("C-r"       . swiper)))

(use-package fzf
  :bind
  ("C-c f" . fzf)
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        fzf/grep-command "rg --no-heading -nH"
        ;; fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

;; avoid call tramp and input path duplicated
;; sudo-edit (Melpa)
(use-package sudo-edit
  :defer t)

;; Files Management & Auto Save
;; Dired (Built-in)
(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-Afhlv")
  :config
  (use-package ivy-dired-history
    :general
    (:keymaps 'dired-mode-map
              "," 'dired))
  (use-package nerd-icons-dired
    :after (dired nerd-icons)
    :hook (dired-mode . nerd-icons-dired-mode)))

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
  (setq recentf-max-saved-items 100)
  (setq  recentf-exclude '("/tmp/"
                           "/ssh:"
                           "/sudo:"
                           ;; ctags
                           "/TAGS$"
                           ;; global
                           "/GTAGS$"
                           "/GRAGS$"
                           "/GPATH$"
                           ;; binary
                           "\\.mkv$"
                           "\\.mp[34]$"
                           "\\.avi$"
                           "\\.pdf$"
                           "\\.docx?$"
                           "\\.xlsx?$"
                           ;; sub-titles
                           "\\.sub$"
                           "\\.srt$"
                           "\\.ass$"
                           ;; no-littering-var-directory
                           ;; `no-littering-var-directory` used absolute path expanded from `user-emacs-directory`
                           ;; But `recentf-list` will save paths with prefix `~/`.
                           ;; So use literal directory here.
                           "~/.config/emacs/var/")))

;; bookmark (Built-in)
(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-save-flag     t))

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

;; alternative to the built-in help
;; helpful (Melpa)
(use-package helpful
  :after counsel
  :init
  (setq counsel-describe-variable-function #'helpful-variable
        counsel-describe-function-function #'helpful-callable
        counsel-describe-symbol-function   #'helpful-symbol)
  :general
  (:prefix "C-x h"
           "k" #'helpful-key
           "F" #'helpful-function
           "C" #'helpful-command)
  ("C-c C-d" #'helpful-at-point))

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
           ("Text"       (or
                          (mode . org-mode)
                          (mode . markdown)
                          (mode . text-mode)))
           ("Config"     (or
                          (mode . yaml-mode)
                          (mode . ssh-config-mode)
                          (derived-mode . conf-mode)
                          (derived-mode . conf-space-mode)))
           ("Program"    (or
                          (derived-mode . prog-mode)
                          (mode . emacs-lisp-mode)
                          (mode . python-mode)
                          (mode . html-mode)
                          (mode . javascript-mode)
                          (mode . css-mode)
                          (mode . java-mode)
                          (mode . haskell-mode)
                          (mode . lisp-mode)
                          (mode . erlang-mode)))
           ("Help"       (or
                          (name . "^\\*Help\\*$")
                          (name . "^\\*Apropos\\*$")
                          (name . "^\\*info\\*$")
                          (name . "^\\*helpful")
                          (name . "^\\*Disabled Command\\*$")
                          (mode . helpful-mode)))
           ("Youdao"     (or
                          (name . "^\\*Youdao Dictionary\\*$")
                          (mode . youdao-dictionary-mode)))
           ("Magit-main" (and
                          (name . "^magit:")
                          (mode . magit-mode)))
           ("Magit-etc"  (and
                          (name . "^magit-")
                          (derived-mode . magit-mode)))
           ("GNUs"       (or
                          (mode . message-mode)
                          (mode . bbdb-mode)
                          (mode . gnus-group-mode)
                          (mode . gnus-summary-mode)
                          (mode . gnus-article-mode)
                          (name . "^\\.bbdb$")
                          (name . "^\\.newsrc-dribble")))
           ("Custom"     (or
                          (name . "^\\*Customize")
                          (mode . custom-mode))))))
  (setq ibuffer-show-empty-filter-groups   nil
        ibuffer-default-sorting-mode       'filename/process)
  (use-package nerd-icons-ibuffer
    :after (ibuffer nerd-icons)
    :hook ((ibuffer-mode . nerd-icons-ibuffer-mode)))
  :bind
  ;; This command is for viewing all buffers.
  ;; Viewing specify buffers in current project by "projectile-ibuffer"
  ;; See ./etc/init-dev-tools.el: projectile
  (("C-x C-S-b"     . ibuffer)))

;; ace-window (Melpa)
(use-package ace-window
  :config
  (defun graphic-p ()
    "Determine whether the current environment is a graphical environment."
    (if (display-graphic-p)
        t))

  (setq graphic-only-plugins-setting ())
  (push '(ace-window-posframe-mode t)
        graphic-only-plugins-setting)

  (if (not (graphic-p))
      (add-hook 'after-make-frame-functions
                (lambda (new-frame)
                  (select-frame new-frame)
                  (dolist (elisp-code graphic-only-plugins-setting)
                    (eval elisp-code))))
    (dolist (elisp-code graphic-only-plugins-setting)
      (eval elisp-code)))

  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground
                   :height 5.0
                   :weight ultra-bold
                   )))))
  (setq aw-keys       '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
        aw-scope      'frame
        aw-background t)
  (setq aw-ignore-current          nil
        aw-dispatch-always         nil
        aw-minibuffer-flag         t
        ;; aw-char-position           'top-left
        aw-dispatch-when-more-than 3)
  (setq aw-dispatch-alist
        '((?x aw-delete-window              "  Ace - Delete Window")
          (?m aw-swap-window                "  Ace - Swap Window")
          (?M aw-move-window                "  Ace - Move Window")
          (?j aw-switch-buffer-in-window    "  Ace - Select Buffer")
          (?n aw-flip-window                "  Ace - Move Window")
          (?u aw-switch-buffer-other-window "  Ace - Switch Buffer Other Window")
          (?c aw-split-window-fair          "  Ace - Split Fair Window")
          (?v aw-split-window-vert          "  Ace - Split Vert Window")
          (?b aw-split-window-horz          "  Ace - Split Horz Window")
          (?o delete-other-windows          "  Ace - Maximize Window")
          (?? aw-show-dispatch-help)))
  (with-eval-after-load 'magit
    (defun my/split-then-magit ()
      (interactive)
      (split-window-horizontally)
      (magit-status)))
  :general
  ("C-M-\'" 'ace-window)
  (:prefix "C-x"
           "1"     'delete-other-windows
           "40"    'ace-window
           "41"    'ace-swap-window
           "4v"    'split-window-vertically
           "4h"    'split-window-horizontally
           "4s"    'ace-swap-window
           "4d"    'ace-delete-window
           "4f"    'find-file-other-window
           "4b"    'switch-to-buffer-other-window
           "4g"    'my/split-then-magit
           "4 SPC" 'balance-windows))

;; shackle (Melpa)
(use-package shackle
  :hook
  (after-init . shackle-mode)
  :init
  (setq shackle-default-rule nil
        shackle-select-reused-windows t
        shackle-default-size 0.35
        shackle-default-alignment 'below
        shackle-rules
        '((magit-status-mode          :select t   :inhibit-window-quit nil :same t)
          (magit-log-mode             :select t   :inhibit-window-quit nil :same t)
          (magit-revision-mode        :select t   :inhibit-window-quit nil :align right :size 0.45)
          (help-mode                  :select t   :inhibit-window-quit nil :align right :size 0.40)
          (helpful-mode               :select t   :align right :size 0.40 :autoclose t)
          (comint-mode                :select t   :align t   :size 0.40)
          (grep-mode                  :select t   :align t)
          (rg-mode                    :select t   :align t)
          (ivy-occur-grep-mode        :select t   :align right :size 0.40)
          (youdao-dictionary-mode     :select t   :inhibit-window-quit nil :align right :size 0.30)
          ("*diff-hl*"                :select nil :align below :size 0.40)
          ("*vc-diff*"                :select t   :align below :size 0.40)
          ("^\\*vc-.*\\*$"            :regexp t :size 0.3 :align 'below :autoclose t)
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


(use-package flycheck-aspell
  :after (flycheck)
  :hook
  (prog-mode . flyspell-prog-mode)
  :config
  ;; If you want to check TeX/LaTeX/ConTeXt buffers
  (add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
  ;; If you want to check Markdown/GFM buffers
  (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
  ;; If you want to check HTML buffers
  (add-to-list 'flycheck-checkers 'html-aspell-dynamic)
  ;; If you want to check XML/SGML buffers
  (add-to-list 'flycheck-checkers 'xml-aspell-dynamic)
  ;; If you want to check Nroff/Troff/Groff buffers
  (add-to-list 'flycheck-checkers 'nroff-aspell-dynamic)
  ;; If you want to check Texinfo buffers
  (add-to-list 'flycheck-checkers 'texinfo-aspell-dynamic)
  ;; If you want to check comments and strings for C-like languages
  (add-to-list 'flycheck-checkers 'c-aspell-dynamic)
  ;; If you want to check message buffers
  (add-to-list 'flycheck-checkers 'mail-aspell-dynamic)

  ;; Because Aspell does not support Org syntax, the user has
  ;; to define a checker with the desired flags themselves.
  ;; (flycheck-aspell-define-checker "erlang"
  ;;   "Erlang" ("--add-filter" "url")
  ;;   (erlang-mode))
  ;; (add-to-list 'flycheck-checkers 'erlang-aspell-dynamic)
  )

(use-package edit-server
  :ensure t
  :commands edit-server-start
  :init (if after-init-time
            (edit-server-start)
          (add-hook 'after-init-hook
                    #'(lambda() (edit-server-start))))
  :config (setq edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (top . 200)
                  (left . 200)
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . t)
                  (window-system . x))))


(provide 'init-base)

;;; init-base.el ends here
