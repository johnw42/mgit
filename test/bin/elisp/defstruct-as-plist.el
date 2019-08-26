;; -*- no-byte-compile: t -*-
(defmacro defstruct-as-plist (name &rest members)
  (let ((struct-name (symbol-name name)))
    `(progn
       (defalias ',(intern (concat struct-name "-p")) 'listp)
       (defun ,(intern (concat "make-" struct-name)) (&rest args)
         (assert (zerop (mod (length args) 2)))
         args)
       ,@(mapcar
          (lambda (member)
            (let ((member-name (symbol-name member)))
              `(defun ,(intern (concat struct-name "-" member-name)) (plist)
                 (plist-get plist ,(intern (concat ":" member-name))))))
          members))))

(provide 'defstruct-as-plist)

;; Local Variables:
;; no-byte-compile: t
;; End:
