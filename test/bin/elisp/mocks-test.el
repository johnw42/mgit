;; -*- no-byte-compile: t -*-
(require 'unit-test-core)
(require 'mocks)

(unit-test-suite mocks-test
  (around (with-mocks (f to-stub to-verify) unit-test))

  (test-no-stubs
   (assert-nil (to-stub)))

  (test-stubs
   (mocks-stub
     (to-stub :return 'stub-result1)
     (to-stub :return 'stub-result2))

   (assert-eq 'stub-result2 (to-stub)))

  (test-stubs-with-args
   (mocks-stub
     (to-stub :if-args-equal '(1)
              :return 'stub-result1)
     (to-stub :if-args-equal '(2)
              :return 'stub-result2))

   (assert-nil (to-stub))
   (assert-eq 'stub-result2 (to-stub 2))
   (assert-eq 'stub-result1 (to-stub 1)))

  (test-verify-simple
   (f)
   (mocks-verify (f))
   (expect-error error
     (mocks-verify (f))))

  (test-verify-twice
   (f)
   (mocks-verify (f))
   (expect-error error
     (mocks-verify (f)))
   (f)
   (mocks-verify (f))
   (expect-error error
     (mocks-verify (f))))

  (test-verify-allows-extra-calls
   (f)
   (f)
   (f)
   (mocks-verify (f))
   (expect-error error
     (mocks-verify (f))))

  (test-verify-pass
   (f)
   (f)
   (mocks-verify (f :times 2))
   (expect-error error (mocks-verify (f :times 2))))

  (test-verify-args-equal-pass
   (f 1 2 3)
   (mocks-verify (f :args-equal '(1 2 3))))

  (test-verify-args-equal-fail
   (f 2 4 6)
   (expect-error error
     (mocks-verify (f :args-equal '(1 2 3)))))

  ;; (test
  ;;  (let (stub-result)
  ;;    (with-mocks (to-verify to-stub)
  ;;      (mocks-stub
  ;;        (to-stub :return 'stub-result1)
  ;;        (to-stub :return 'stub-result2))

  ;;      (setq stub-result (to-stub 1 2 3))
  ;;      (to-verify 1 2 3)

  ;;      (mocks-verify
  ;;        (to-verify :args-equal '(1 2 3)))
  ;;      (assert-eq 'stub-result2 stub-result))))

  )

;; Local Variables:
;; no-byte-compile: t
;; End:
