;; -*- no-byte-compile: t -*-
(require 'unit-test-core)
(require 'unit-test-utils)

(unit-test-suite unit-test-utils-test
  (test-unit-test-file-p
   (assert (unit-test-file-p "/a/b/foo-test.el"))
   (assert-not (unit-test-file-p "/a/b/foo.el")))

  (test-unit-test-test-file
   (assert-equal "/a/b/foo-test.el"
                 (unit-test-test-file "/a/b/foo-test.el"))
   (assert-equal "/a/b/foo-test.el"
                 (unit-test-test-file "/a/b/foo.el")))


  (test-unit-test-tested-file
   (assert-equal "/a/b/foo.el"
                 (unit-test-tested-file "/a/b/foo-test.el"))
   (assert-equal "/a/b/foo.el"
                 (unit-test-tested-file "/a/b/foo.el")))

  (test-unit-test-feature-name
   (assert-equal "foo"
                 (unit-test-feature-name "/a/b/foo-test.el"))
   (assert-equal "foo"
                 (unit-test-feature-name "/a/b/foo.el"))))

;; Local Variables:
;; no-byte-compile: t
;; End:
