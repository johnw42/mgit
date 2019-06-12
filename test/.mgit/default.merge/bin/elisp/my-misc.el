;; -*- no-byte-compile: t -*-
(defun my-copy-path ()
  (interactive)
  "Copy the current buffer's path to the system clipboard."
  (kill-new (buffer-file-name))
  ;;(x-set-selection nil (buffer-file-name))
  )

(provide 'my-misc)

;; Local Variables:
;; no-byte-compile: t
;; End:
