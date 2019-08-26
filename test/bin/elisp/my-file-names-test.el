;; -*- no-byte-compile: t -*-
(require 'my-file-names)
(require 'unit-test-core)

(unit-test-suite my-file-names-test
  (test-file-name-prefix-p
   (assert (file-name-prefix-p "/foo" "/foo/bar"))
   (assert (file-name-prefix-p "/foo/" "/foo/bar"))
   (assert (file-name-prefix-p "/foo/bar" "/foo/bar"))
   (assert (file-name-prefix-p "foo" "foo/bar"))
   (assert (file-name-prefix-p "foo/" "foo/bar"))
   (assert (file-name-prefix-p "foo/bar" "foo/bar"))
   (assert-nil (file-name-prefix-p "/fo" "/foo/bar"))
   (assert-nil (file-name-prefix-p "/foo/ba" "/foo/bar"))
   (assert-nil (file-name-prefix-p "fo" "foo/bar"))
   (assert-nil (file-name-prefix-p "foo/ba" "foo/bar")))

  (test-file-name-suffix-p
   (assert (file-name-suffix-p "bar" "/foo/bar"))
   (assert (file-name-suffix-p "/foo/bar" "/foo/bar"))
   (assert (file-name-suffix-p "bar" "foo/bar"))
   (assert (file-name-suffix-p "foo/bar" "foo/bar"))
   (assert (file-name-suffix-p "bar/" "/foo/bar"))
   (assert (file-name-suffix-p "bar/" "foo/bar"))
   (assert (file-name-suffix-p "bar/" "/foo/bar/"))
   (assert (file-name-suffix-p "bar/" "foo/bar/"))
   (assert-nil (file-name-suffix-p "/bar" "/foo/bar"))
   (assert-nil (file-name-suffix-p "/bar" "foo/bar"))
   (assert-nil (file-name-suffix-p "ar" "foo/bar"))
   (assert-nil (file-name-suffix-p "oo/bar" "foo/bar"))))

;; Local Variables:
;; no-byte-compile: t
;; End:
