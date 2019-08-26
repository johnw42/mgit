(defun before-compile-hook (&rest _)
  "Saves the current buffer if it is associated with a file."
  (interactive)
  (when buffer-file-name
    (save-buffer)))

(provide 'before-compile-hook)
