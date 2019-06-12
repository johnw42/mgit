;; -*- no-byte-compile: t -*-
;; Prevent clicking in the echo area from bringing up a useless view
;; of the *Messages* buffer.

(defvar in-mouse-drag-region nil)

(defadvice display-buffer (after my-mouse-drag-region activate)
  (when in-mouse-drag-region
    ;;(message "recentering")
    (recenter -1)))

(defadvice mouse-drag-region (around my-mouse-drag-region activate)
  (let ((in-mouse-drag-region t))
    ;;(message "mouse-drag-region")
    ad-do-it))

(provide 'my-mouse-drag-region)

;; Local Variables:
;; no-byte-compile: t
;; End:
