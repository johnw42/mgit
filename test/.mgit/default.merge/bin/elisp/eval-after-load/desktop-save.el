;; -*- no-byte-compile: t; -*-
;; Copyright 2012 Google Inc. All Rights Reserved.
;; Author: jrw@google.com (John Williams)

(defvar my-desktop-save-timer nil)

(add-hook
 'auto-save-hook
 (defun my-desktop-save-auto-save-hook ()
   (when desktop-save-mode
     (when my-desktop-save-timer
       (cancel-timer my-desktop-save-timer))
     (setq my-desktop-save-timer
           (run-with-idle-timer
            60 nil
            (lambda ()
              ;; Call `desktop-save-in-desktop-dir' without disturbing
              ;; the echo area/minibuffer.
              (with-temp-message "Saving desktop..."
                (desktop-save-in-desktop-dir))))))))
