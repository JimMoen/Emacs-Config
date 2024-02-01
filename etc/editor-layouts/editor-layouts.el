;;; editor-layouts.el --- Workspace feature support -*- lexical-binding: t -*-

;; Copyright (C) 2021  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Package-Requires: (persp-mode)
;; Keywords: layouts/Workspace


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

;; Support layout/Workspace feature
;; Forked from Spacemacs/layouts.
;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/%2Bspacemacs/spacemacs-layouts/README.org
;; Forked form tshu-w Emacs dot file.
;; https://github.com/tshu-w/.emacs.d

;;; Code:

;; persp-mode (Melpa)
(use-package persp-mode
  :init
  (defconst default-persp-name "Main")
  (setq persp-add-buffer-on-after-change-major-mode 'free
        persp-nil-name                              default-persp-name
        persp-auto-resume-time                      1
        persp-is-ibc-as-f-supported                 nil
        persp-set-last-persp-for-new-frames         nil)

  (when (member "--persp-r" command-line-args)
    (setq persp-auto-resume-time 1)
    (delete "--persp-r" command-line-args))

  (when (member "--persp-q" command-line-args)
    (setq persp-auto-resume-time -1
          persp-auto-save-opt 0)
    (delete "--persp-q" command-line-args))

  :hook
  (after-init . persp-mode)
  :custom
  (persp-keymap-prefix "w")
  :config
  (setq persp-kill-foreign-buffer-behaviour nil)
  ;; fix persp and ivy-posframe conflict
  (defun persp-load-state-from-file@after (&optional _ _ _)
    (posframe-delete-all))
  (advice-add 'persp-load-state-from-file :after #'persp-load-state-from-file@after)

  (defun current-persp-name ()
    "Get name of the current perspective."
    (safe-persp-name (get-frame-persp)))

  (defvar last-selected-persp default-persp-name
    "Previously selected layout.")
  (defun save-last-selected-perspective (_ &optional _ _)
    (setq last-selected-persp persp-last-persp-name))

  (defun alternate-persp ()
    "Open the previously selected layout, if it exists."
    (interactive)
    (unless (eq 'non-existent
                (gethash last-selected-persp
                         *persp-hash* 'non-existent))
      (persp-switch last-selected-persp)))

  (defun kill-current-persp ()
    "Kill current perspective"
    (interactive)
    (persp-kill (current-persp-name)))

  (defun format-persp-name (name pos)
    "Format the perspective name given by NAME for display in mode-line."
    (let* ((persp-name (if (file-directory-p name)
                           (file-name-nondirectory (directory-file-name name))
                         name))
           (string-name (format "%s" persp-name))
           (current (equal name (current-persp-name)))
           (caption (concat (number-to-string (if (eq 9 pos) 0 (+ 1 pos)))
                            ":" string-name)))
      (if current
          (propertize (concat "[" caption "]") 'face 'warning)
        caption)))

  (defun format-persp-states ()
    "Return an one liner string containing all the perspective names."
    (let ((persp-list (or (persp-names-current-frame-fast-ordered)
                          (list persp-nil-name))))
      (concat " "
              (mapconcat (lambda (persp)
                           (format-persp-name
                            persp (cl-position persp persp-list)))
                         persp-list " | "))))

  ;; Fixme!! Move me to "core-funcs.el" or other files.
  (defun echo (msg &rest args)
    "Display MSG in echo-area without logging it in *Messages* buffer."
    (interactive)
    (let ((message-log-max nil))
      (apply 'message msg args)))


  (defun show-persp-hint ()
    "Show persp hint."
    (interactive)
    (echo (format-persp-states)))

  (defun persp-switch@after (_ &optional _ _)
    (show-persp-hint))

  (defun switch-persp-by-pos (pos)
    "Switch to perspective of position POS.
  If POS has no layout, ask the user if a new layout should be created."
    (let ((persp-to-switch
           (nth pos (persp-names-current-frame-fast-ordered))))
      (if persp-to-switch
          (persp-switch persp-to-switch)
        (persp-switch nil))))

  ;; Define all `switch-to-persp-X' functions
  (dolist (i (number-sequence 9 0 -1))
    (eval `(defun ,(intern (format "switch-to-persp-%s" i)) nil
             ,(format "Switch to perspective %s.\n%s"
                      i "See `switch-persp-by-pos' for details.")
             (interactive)
             (switch-persp-by-pos ,(if (eq 0 i) 9 (- i 1))))))

  (defun goto-default-persp ()
    "Go to `default-persp-name' perspective"
    (interactive)
    (when default-persp-name
      (persp-switch default-persp-name)))

  (defun move-element-left (element list)
    "Move ELEMENT one step to the left in LIST."
    (let (value)
      (dolist (name list value)
        (if (and (equal name element) (car value))
            (setq value (cons (car value) (cons name (cdr value))))
          (setq value (cons name value))))
      (nreverse value)))

  (defun move-element-right (element list)
    "Move ELEMENT one step to the right in LIST."
    (nreverse (move-element-left element (reverse list))))

  (defun move-current-persp-right ()
    "Moves the current perspective one step to the right."
    (interactive)
    (setq persp-names-cache (move-element-right
                             (current-persp-name)
                             persp-names-cache))
    (show-persp-hint))

  (defun move-current-persp-left ()
    "Moves the current perspective one step to the left."
    (interactive)
    (setq persp-names-cache (move-element-left
                             (current-persp-name)
                             persp-names-cache))
    (show-persp-hint))

  ;; Persp and Projectile integration
  (defun persp-switch-project (name &optional body)
    "Switch to project persp with adding project buffers and execute BODY."
    (let ((persp-reset-windows-on-nil-window-conf nil)
          (persp-already-exists (persp-with-name-exists-p name)))
      (persp-switch name)
      (condition-case nil
          (eval body)
        (quit (persp-kill-without-buffers name)))
      (unless persp-already-exists
        (let ((persp (persp-get-by-name name)))
          (when (persp-p persp)
            (persp-add-buffer (projectile-project-buffers
                               (expand-file-name name))
                              persp nil nil))))))

  ;; Persp and Ivy integration
  (defun persp-not-contains-buffer-p (buffer)
    "Return non-nil if current perspective doesn't contain BUFFER."
    (not (persp-contain-buffer-p buffer)))

  (defun ivy-switch-buffers-not-restricted ()
    (interactive)
    (let ((ivy-ignore-buffers
           (remove #'persp-not-contains-buffer-p ivy-ignore-buffers)))
      (ivy-switch-buffer)))

  (defun ivy-persp-switch-project-action (project)
    "Default action for `ivy-persp-switch-project'."
    (persp-switch-project
     project `(counsel-projectile-find-file-action ,project)))

  (defun ivy-persp-switch-project-open-dired (project)
    (interactive)
    (persp-switch-project
     project `(dired ,project)))

  (defun ivy-persp-switch-project (arg)
    "Select a project layout using Ivy."
    (interactive "P")
    (require 'counsel-projectile)
    (ivy-read "Switch to Project Perspective: "
              (if (projectile-project-p)
                  (cons (abbreviate-file-name (projectile-project-root))
                        (projectile-relevant-known-projects))
                projectile-known-projects)
              :action #'ivy-persp-switch-project-action
              :caller #'ivy-persp-switch-project))

  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers #'persp-not-contains-buffer-p)
    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil)))))

  (with-eval-after-load 'counsel-projectile
    (ivy-set-actions 'ivy-persp-switch-project
                     '(("d" ivy-persp-switch-project-open-dired "dired"))))

  (advice-add 'persp-activate :before #'save-last-selected-perspective)
  (advice-add 'persp-switch   :after  #'persp-switch@after)

  (with-eval-after-load 'which-key
    ;; rename the switch-to-persp-1 entry, to 1..10
    (push '(("\\(.*\\)1" . "switch-to-persp-1") .
            ("\\11..0" . "switch-to-persp 1..10"))
          which-key-replacement-alist)

    ;; hide the "[0,2-9] -> switch-to-persp-[0,2-9]" entries
    (push '((nil . "switch-to-persp-[0,2-9]") . t)
          which-key-replacement-alist))
  :bind
  (("C-x b"       . persp-switch-to-buffer)   ;; Switch buffer in current persp.
   ("C-x k"       . persp-kill-buffer)        ;; Kill buffer in current persp.
   ("C-x K"       . kill-buffer)              ;; Kill specify buffer global.
   ("C-x w s"     . persp-switch)
   ("C-x w P"     . ivy-persp-switch-project)
   ("C-x w TAB"   . alternate-persp)
   ("C-x w ["     . move-current-persp-left)
   ("C-x w ]"     . move-current-persp-right)
   ("C-x w r"     . presp-remove-buffer)
   ("C-x w R"     . persp-rename)
   ("C-x w a"     . persp-add-buffer)
   ("C-x w i"     . persp-import-buffers)
   ("C-x w d"     . goto-default-persp)
   ("C-x w c"     . kill-current-persp)
   ("C-x w C"     . persp-kill)
   ("C-x w l"     . persp-load-state-from-file)
   ("C-x w N"     . persp-add-new)
   (:map projectile-command-map
         ("p". ivy-persp-switch-project))))


(provide 'editor-layouts)

;;; editor-layouts.el ends here
