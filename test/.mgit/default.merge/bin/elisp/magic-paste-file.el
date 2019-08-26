;; -*- no-byte-compile: t -*-
;; -*- lexical-binding: t; -*-

(defadvice mouse-yank-primary (around magic-paste-file activate)
  (let ((selection (x-get-selection-value)))
    ad-do-it)
  )

(provide 'magic-paste-file)

;; Local Variables:
;; no-byte-compile: t
;; End:
