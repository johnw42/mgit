;; -*- no-byte-compile: t -*-
;; -*- lexical-binding: t; ; ; ; ; ; -*-
(elide/ns elide-analyze
  (:import elide))

(elide/defstruct form
  type chilren)

(elide/defstruct form-env
  type)

(defun elide-analyze/analyze-expr (form &optional env)
  (let (type children)
    (cond
     ((or (null form)
          (eq 't form)
          (keywordp form))
      (setq type )
      )
     )
    (make-form :orig form
               :type type
               :children children)))

(defun elide-analyze/extract-form (form)
  (case (form-type form)
    ))

(provide 'elide-analyze)

;; Local Variables:
;; no-byte-compile: t
;; End:
