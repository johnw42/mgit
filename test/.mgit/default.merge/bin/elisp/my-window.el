;; -*- no-byte-compile: t -*-
(defun my-delete-other-windows ()
  (interactive)
  (let ((num-windows (length (window-list))))
    (delete-other-windows-vertically)
    (when (= (length (window-list)) num-windows)
      (delete-other-windows))))

(provide 'my-window)

;; Local Variables:
;; no-byte-compile: t
;; End:
