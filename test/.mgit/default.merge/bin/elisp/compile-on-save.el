;;;###autoload
(define-minor-mode compile-on-save-mode
  "Minor mode to compile an Emacs Lisp file whenever it is saved."
  :global t
  :lighter " CS"
  (if compile-on-save-mode
      (add-hook 'after-save-hook
                'compile-on-save-after-save-hook)
    (remove-hook 'after-save-hook
                 'compile-on-save-after-save-hook)))

;;;###autoload(put 'no-compile-on-save 'safe-local-variable 'booleanp)
(defvar no-compile-on-save nil
  "Set this variable to t prevent a file from being automatically
  compiled when it is saved.")

(defun compile-on-save-after-save-hook ()
  (when (and (not no-compile-on-save)
             (not no-byte-compile)
             (derived-mode-p 'emacs-lisp-mode))
    (let* ((compiled-name (byte-compile-dest-file buffer-file-name))
           (names-to-check (list buffer-file-name
                                 compiled-name))
           already-loaded)
      (dolist (entry load-history)
        (let ((filename (car entry)))
          (when (member filename names-to-check)
            (setq already-loaded t))))
      (when (boundp 'watch-elc-files-file-times)
        (remhash compiled-name watch-elc-files-file-times))
      (byte-recompile-file (buffer-file-name)
                           nil nil already-loaded))))

(provide 'compile-on-save)
