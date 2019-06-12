;; -*- no-byte-compile: t -*-
;; Utility functions for plists.
(eval-when-compile (require 'cl))

(defun plist-put* (plist &rest props-and-vals)
  "Like `plist-put', but accepts multiple pairs of keys and
values."
  (while props-and-vals
    (setq plist (plist-put
                 plist
                 (car props-and-vals)
                 (cadr props-and-vals)))
    (setq props-and-vals (cddr props-and-vals)))
  plist)

(defmacro plist-setf (plist prop val &rest props-and-vals)
  "Updates the plist in location PLIST by setting PROP to VAL."
  (if props-and-vals
      `(callf plist-put* ,plist ,prop ,val ,@props-and-vals)
    `(callf plist-put ,plist ,prop ,val)))

(provide 'plist)

;; Local Variables:
;; no-byte-compile: t
;; End:
