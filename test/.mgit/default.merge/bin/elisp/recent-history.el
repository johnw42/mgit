;; -*- no-byte-compile: t -*-
(defadvice add-to-history (after
                           recent-history
                           (history-var newelt &optional maxelt keep-all)
                           activate compile)
  )

(when (eq this-command 'eval-defun)
  (let ((history-length 5)
        (history-delete-duplicates nil)
        (the-history nil))
    (add-to-history 'the-history "item0")
    (add-to-history 'the-history "item1")
    (add-to-history 'the-history "item2")
    (assert (equal the-history '("item2" "item1" "item0")))
    (add-to-history 'the-history "item0")
    (assert (equal the-history '("item0" "item2" "item1" "item0")))
    (message "the-history: %S" the-history))
  )

(provide 'recent-history)

;; Local Variables:
;; no-byte-compile: t
;; End:
