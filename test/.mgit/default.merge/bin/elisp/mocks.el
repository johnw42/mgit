;; -*- no-byte-compile: t -*-
(require 'cl)
(require 'my-util)
(require 'unit-test-core)

;; (defvar mocks-mocked-functions)

;; (defun mocks-function-called (func args)
;;   )

;; (defmacro mocks-install-mock (func)
;;   `(unless (assq ',func mocks-mocked-functions)
;;      (push (list ',func (symbol-function ',func))
;;            mocks-mocked-functions)
;;      (defun ,func (&rest mocks-args)
;;        (mocks-function-called ',func mocks-args))))

;; (defmacro mocks-when (funcall)
;;   `(mocks-install-mock ',(car funcall)))

;; (defmacro mocks-verify (funcall)
;;   `(mocks-install-mock ',(car funcall)))

;; (defmacro with-mocks (&body)
;;   (declare (indent 1))
;;   `(let ((mocks-mocked-functions))
;;      (unwind-protect
;;          (progn ,@body)
;;        (dolist (entry (mocks-mocked-functions))
;;          (split-let* (((func old-value) entry))
;;            (setf (symbol-function func) old-value))))))

(defun* mocks-stub-function (f &key
                               (if-args-equal 'any-args)
                               return
                               return-list)
  (puthash f (cons (list if-args-equal
                         return
                         return-list)
                   (gethash f mocks-stubs))
           mocks-stubs))

(defun mocks-call-function (f args)
  (puthash f (cons args (gethash f mocks-recorded-calls))
           mocks-recorded-calls)
  (catch 'stub-result
    (let ((stubs (gethash f mocks-stubs)))
      (dolist (stub stubs)
        (split-let* (((if-args-equal return return-list) stub))
          (when (or (eq 'any-args if-args-equal)
                    (equal args if-args-equal))
            (throw 'stub-result return)))))))

(defun* mocks-verify-function (f &key
                                 (args-equal 'any-args)
                                 times
                                 min-times
                                 max-times)
  (assert (not (and times
                    (or min-times max-times))))
  (when (not (or times min-times max-times))
    (setq min-times 1))
  (let ((calls (gethash f mocks-recorded-calls))
        (times-matched 0))
    (dolist (call calls)
      (when (or (eq 'any-args args-equal)
                (equal call args-equal))
        (incf times-matched)))
    (when (and times
               (not (= times times-matched)))
      (error "Expected %S calls to %S, got %S"
             times f times-matched))
    (when (and min-times
               (< times-matched min-times))
      (error "Expected at least %S calls to %S, got %S"
             min-times f times-matched))
    (when (and max-times
               (> times-matched max-times))
      (error "Expected at most %S calls to %S, got %S"
             max-times f times-matched))))

(defun mocks-stub* (body)
  (eval
   (let (flet-bindings)
     (dolist (name mocks-mocked-functions)
       (push `(,name (&rest args) (apply 'mocks-stub-function ',name args))
             flet-bindings))
     `(flet (,@flet-bindings) ,@body))))

(defmacro mocks-stub (&rest body)
  (declare (indent 0))
  `(mocks-stub* ',body))

(defun mocks-verify* (body)
  (eval
   (let (flet-bindings)
     (dolist (name mocks-mocked-functions)
       (push `(,name (&rest args) (apply 'mocks-verify-function ',name args))
             flet-bindings))
     `(unwind-protect
          (progn
            (dolist (f mocks-mocked-functions)
              (puthash f (nreverse (gethash f mocks-recorded-calls))
                       mocks-recorded-calls))
            (flet (,@flet-bindings) ,@body))
        (clrhash mocks-recorded-calls)))))

(defmacro mocks-verify (&rest body)
  (declare (indent 0))
  `(mocks-verify* ',body))

(defvar mocks-mocked-functions)
(defvar mocks-recorded-calls)
(defvar mocks-stubs)

(defmacro with-mocks (mocked-functions &rest body)
  (declare (indent 1))
  (let (flet-bindings)
    (dolist (name mocked-functions)
      (push
       `(,name (&rest args) (mocks-call-function ',name args))
       flet-bindings))
    `(let ((mocks-mocked-functions ',mocked-functions)
           (mocks-recorded-calls (make-hash-table :test 'eq))
           (mocks-stubs (make-hash-table :test 'eq)))
       (flet (,@flet-bindings) ,@body))))

(provide 'mocks)

;; Local Variables:
;; no-byte-compile: t
;; End:
