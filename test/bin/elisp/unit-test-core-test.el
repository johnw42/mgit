;; -*- no-byte-compile: t -*-
(require 'unit-test-core)

(defvar void-var)

(unit-test-suite unit-test-suite-test-suite-1
  (let ((debug-on-error nil)))

  (test-sanity
   (let ((test-count 0))
     (unit-test-suite dummy-suite
       (test-1 (incf test-count))
       (test-2 (incf test-count)))
     (assert-equal 2 test-count)))

  (test-failure
   (unit-test-suite dummy-suite
     (test-1 (error "test failed"))))

  (test-set-up-clause
   (let ((set-up-count 0))
     (unit-test-suite dummy-suite
       (set-up (incf set-up-count))
       (test-1)
       (test-2 (error "failed")))
     (assert-equal 2 set-up-count)))

  (test-tear-down-clause
   (let ((tear-down-count 0))
     (unit-test-suite dummy-suite
       (tear-down (incf tear-down-count))
       (test-1)
       (test-2 (error "failed")))
     (assert-equal 2 tear-down-count)))

  (test-bad-clause-type
   (expect-error error
     (flet ((unit-test-message (&rest args)))
       (unit-test-suite dummy-suite
         (xyzzy)
         (test-1)))))

  ;; (test-save-match-data
  ;;  (let (tests-failed)
  ;;    (save-match-data
  ;;      (string-match "a" "abc")
  ;;      (flet ((unit-test-failed (name err) (push name tests-failed)))
  ;;        (unit-test-suite dummy-suite
  ;;          (test-1 (string-match "b" "abc")))))
  ;;    (assert-equal '(test-1) tests-failed)))

  (test-unit-test-suite
   (let (tests-passed
         tests-failed
         tests-started
         (outer-var 0)
         (var-tests-ran 0)
         (set-up-count 0)
         (tear-down-count 0))
     (flet ((unit-test-passed (name) (push name tests-passed))
            (unit-test-failed (name err) (push name tests-failed))
            (unit-test-started (name) (push name tests-started))
            (unit-test-message (&rest args)))
       (unit-test-suite unit-test-suite-test
         (around (let ((inner-var 0))
                   unit-test))
         (set-up
          (incf set-up-count)
          (incf inner-var)
          (incf outer-var))
         (tear-down
          (incf tear-down-count)
          (decf outer-var))
         (test-with-failure
          (error "failing"))
         (test-var-1
          (incf var-tests-ran)
          (assert-equal 1 inner-var)
          (assert-equal 1 outer-var))
         (test-var-2
          (incf var-tests-ran)
          (assert-equal 1 inner-var)
          (assert-equal 1 outer-var))))
     (assert-equal 3 set-up-count)
     (assert-equal 3 tear-down-count)
     (assert-equal 0 outer-var)
     (assert-equal 2 var-tests-ran)
     (assert-equal '(test-var-2 test-var-1 test-with-failure)
                   tests-started)
     (assert-equal '(test-var-2 test-var-1)
                   tests-passed)
     (assert-equal '(test-with-failure)
                   tests-failed))))

(unit-test-suite unit-test-suite-test-suite-2

  ;; (test-failure
  ;;  (assert debug-on-error)
  ;;  (error "Should enter debugger here."))

  (test-assert-equal-passed
   (assert-equal 0 0))

  (test-assert-equal-failed
   (expect-error error
     (assert-equal 0 1)))

  (test-assert-nil-passed
   (assert-nil nil))

  (test-assert-nil-failed
   (expect-error error
     (assert-nil t)))

  (test-assert-invariant-passed
   (let ((x 0)) (assert-invariant x)))

  (test-assert-invariant-failed
   (expect-error error
     (let ((x 0))
       (assert-invariant x
         (incf x)))))

  (test-expect-error-succeeded
   (expect-error error
     (error "expected error")))

  (test-expect-error-no-error
   (expect-error error
     (expect-error error
       t)))

  (test-expect-error-wrong-type
   (expect-error error
     (expect-error void-function
       (error "wrong error type"))))

  ;; (test-save-global-state
  ;;  (makunbound 'void-var)
  ;;  (let ((xyzzy 1))
  ;;    (save-global-state ()
  ;;      (setq void-var nil)
  ;;      (setq xyzzy 2))
  ;;    (assert-equal 1 xyzzy)
  ;;    ;; (assert-not (boundp 'void-var))
  ;;    ))

  ;; (test-save-global-state
  ;;  (makunbound 'void-var)
  ;;  (let ((xyzzy 1))
  ;;    (save-global-state ()
  ;;      (setq void-var nil)
  ;;      (setq xyzzy 2))
  ;;    (assert-equal 1 xyzzy)
  ;;    (assert-not (boundp 'void-var))))

  ;; (test-abbrev
  ;;  (unless noninteractive
  ;;    (setq buffer-file-name "/foo/bar.el")
  ;;    (emacs-lisp-mode)
  ;;    (execute-kbd-macro "(unit-test-suite ")
  ;;    (assert-looking-back "(unit-test-suite bar ")))
  )

;; Local Variables:
;; no-byte-compile: t
;; End:
