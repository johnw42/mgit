;; -*- no-byte-compile: t -*-
(require 'load-path-alias)
(require 'unit-test-core)

(unit-test-suite load-path-alias-tests

  (test-load-path-with-aliases-1
   (let ((load-path-alias-list nil)
         (load-path '("a" "b")))
     (assert-equal '("a" "b") (load-path-with-aliases))))

  (test-load-path-with-aliases-2
   (let ((load-path-alias-list
          '(("^x" "y")))
         (load-path '("a" "xb")))
     (assert-equal '("a" "yb" "xb") (load-path-with-aliases)))))

;; Local Variables:
;; no-byte-compile: t
;; End:
