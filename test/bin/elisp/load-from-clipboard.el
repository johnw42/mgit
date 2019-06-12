;; -*- no-byte-compile: t -*-
(global-set-key [switch-frame] 'my-switch-frame-handler)

(defun my-switch-frame-handler ()
  (interactive)
  (message "my-switch-frame-handler"))

(provide 'load-from-clipboard)

;; Local Variables:
;; no-byte-compile: t
;; End:
