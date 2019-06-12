;; -*- no-byte-compile: t -*-
(require 'plist)
(require 'unit-test-core)

(unit-test-suite plist-test

  (test-plist-setf
   (let (plist)
     (assert-nil plist)
     (plist-setf plist 'key1 'value1)
     (assert-eq 'value1 (plist-get plist 'key1))
     (plist-setf plist 'key2 'wrong-value2 'key3 'value3 'key2 'value2)
     (assert-eq 'value1 (plist-get plist 'key1))
     (assert-eq 'value2 (plist-get plist 'key2))
     (assert-eq 'value3 (plist-get plist 'key3)))))

;; Local Variables:
;; no-byte-compile: t
;; End:
