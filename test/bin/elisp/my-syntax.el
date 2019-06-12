;; -*- no-byte-compile: t -*-
(defun syntax-string-start (&optional pos)
  "Test whether POS is within a string according to the current syntax table.
Returns the start position of the string or nil."
  (let ((state (syntax-ppss (or pos (point)))))
    (and (elt state 3)
         (elt state 8))))

(defun synax-comment-start (&optional pos)
  "Test whether POS is within a comment according to the current syntax table.
Returns the start position of the comment or nil."
  (let ((state (syntax-ppss (or pos (point)))))
    (and (elt state 7)
         (elt state 8))))

(defun syntax-normal-p (&optional pos)
  "Test whether the syntax at POS is normal, i.e. not a string or comment."
  (let ((state (syntax-ppss (or pos (point)))))
    (not (elt state 8))))

(provide 'my-syntax)

;; Local Variables:
;; no-byte-compile: t
;; End:
