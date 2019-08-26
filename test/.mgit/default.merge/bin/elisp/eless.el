;; -*- no-byte-compile: t -*-
(defun eless (temp-file-name)
  (let ((proc (frame-parameter (selected-frame) 'client)))
    (switch-to-buffer (generate-new-buffer "*eless*"))
    (insert-file temp-file-name)
    (delete-file temp-file-name)
    (set-buffer-modified-p nil)
    (if (processp proc)
        (progn
          ;; Handle C-x C-c the as if the buffer were opened with
          ;; emacsclient -c <filename>.
          (setq server-buffer-clients (list proc))
          (process-put proc 'buffers
                       (list (current-buffer))))
      (add-hook 'delete-frame-functions
                'eless-delete-frame-function
                nil 'local))
    nil))

(defun eless-delete-frame-function (frame)
  (kill-buffer (current-buffer)))

(provide 'eless)

;; Local Variables:
;; no-byte-compile: t
;; End:
