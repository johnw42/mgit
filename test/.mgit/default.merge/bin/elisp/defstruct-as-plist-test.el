;; -*- no-byte-compile: t -*-
(require 'defstruct-as-plist)
(require 'unit-test-core)

(unit-test-suite defstruct-as-plist-test
  (test-it
   (assert-equal
    '(progn
       (defalias (quote s-p) 'listp)
       (defun make-s (&rest args)
         (assert (zerop (mod (length args) 2)))
         args)
       (defun s-m1 (plist)
         (plist-get plist :m1))
       (defun s-m2 (plist)
         (plist-get plist :m2)))
    (macroexpand '(defstruct-as-plist s m1 m2)))))

;; Local Variables:
;; no-byte-compile: t
;; End:
