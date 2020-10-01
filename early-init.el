;;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;;; File: early-init.el
;;;; Auth: JimMoen
;;;; Code:

;;;; #######################################################
;;;; Coding System Settings
(prefer-coding-system 'utf-8-emacs)
(set-default-coding-systems 'utf-8-emacs)

;;;; Defer garbage collection futher back in the startup process
;;;; Garbage collection threshold set to 20MiB. Which use 2GiB `DATA`.
(setq gc-cons-threshold (* 20 1024 1024)
      gc-cons-percentage 0.1)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;;; #######################################################
;;;; early-init.el ends here
