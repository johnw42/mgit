(defun auto-script-mode-newline ()
  (interactive)
  (if (and (= 1 (line-number-at-pos))
           (buffer-file-name)
           (save-excursion
             (beginning-of-line)
             (looking-at "#! ?/")))
      (progn
        (newline)
        (save-buffer)
        (call-process "chmod" nil nil nil
                      "+x" (buffer-file-name))
        (normal-mode)
        (auto-script-mode 0))
    (newline)))

(defconst auto-script-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'auto-script-mode-newline)
    map))

(define-minor-mode auto-script-mode
  "Minor mode that automatically adds execute permission of
activates an appropriate major mode when a #! line is typed
into a file."
  :lighter " auto-script"
  :keymap auto-script-mode-map)

(provide 'auto-script-mode)
