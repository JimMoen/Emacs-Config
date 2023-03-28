;;; early-init.el --- Early Init File. -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Copyright (C) 2021  JimMoen

;; Author: JimMoen <LnJimMoen@outlook.com>
;; Keywords: early-init


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

;; early-init.el would eval befor init.el

;;; Code:

;; ##########

;; Coding System Settings
(prefer-coding-system 'utf-8-emacs)
(set-default-coding-systems 'utf-8-emacs)

;; Defer garbage collection futher back in the startup process
;; Garbage collection threshold set to 20MiB. Which use 2GiB `DATA`.
(setq gc-cons-threshold (* 20 1024 1024)
      gc-cons-percentage 0.1)

;; Prevent initializing twice
(setq package-enable-at-startup nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(when (and
       (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    ;; add to $EMACS_INITDIR/early-init.el
    (defvar native-comp-deferred-compilation-deny-list nil)
    (setq comp-deferred-compilation nil)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))
;;; early-init.el ends here
