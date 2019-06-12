;; -*- no-byte-compile: t -*-
(require 'cl)
(require 'my-file-names)
(require 'unit-test-runner)

(defvar unit-test-running nil
  "True when unit tests are running.")

(defadvice current-message (around unit-test-core activate)
  (if unit-test-running
      (setq ad-return-value (car unit-test-messages))
    ad-do-it))

(defadvice message (around unit-test-core activate)
  (if unit-test-running
      (let ((unit-test-running nil))
        (push (apply 'format (ad-get-args 0)) unit-test-messages)
        (setq ad-return-value (car unit-test-messages)))
    ad-do-it))

;; (defmacro setq-from-nil (symbol value)
;;   `(progn
;;      (assert (null ,symbol))
;;      (setq ,symbol ,value)))

(defun with-function-name* (function-name form)
  "Run BODY in a temporary function named FUNCTION-NAME.  The
purpose of this macro is to assist debugging by putting the given
function name in the call stack."
  (eval `(flet ((,function-name () ,form)) (,function-name))))

(defmacro with-function-name (function-name-form &rest body)
  "Run BODY in a temporary function named FUNCTION-NAME.  The
purpose of this macro is to assist debugging by putting the given
function name in the call stack."
  (declare (indent 1))
  `(with-function-name* ,function-name-form '(progn ,@body)))

(defun* call-stack ()
  (let ((frame-number 0)
        found-self
        result)
    (while t
      (let ((frame-info (backtrace-frame frame-number)))
        (when (not frame-info)
          (return-from call-stack (nreverse result)))
        (let ((function-name (cadr frame-info)))
          (if found-self
              (push function-name result)
            (if (eq function-name 'call-stack)
                (setq found-self t)))))
      (incf frame-number))))

(defun called-from-eval-defun-p ()
  (eq 'call-interactively
        (cadr (memq 'eval-defun (call-stack)))))

(defun current-interactive-command ()
  (let ((stack (call-stack)))
    (while (and stack
                (not (eq 'call-interactively (cadr stack))))
      (pop stack))
    (car stack)))

(defvar unit-test-suite-name nil)

(defconst unit-test-buffer-name "*unit-test*")

(defvar unit-test-buffer nil)

(defun unit-test-reset-buffer ()
  (let* ((old-buffer (get-buffer unit-test-buffer-name))
         (window-list (and old-buffer (get-buffer-window-list old-buffer)))
         new-buffer)
    (when old-buffer
      (with-current-buffer old-buffer
        (setq buffer-file-name nil))
      (kill-buffer old-buffer))
    (let ((new-buffer (get-buffer-create unit-test-buffer-name)))
      (dolist (window window-list)
        (set-window-buffer window new-buffer))
      new-buffer)))

(defconst unit-test-default-clauses
  '((around
     (let ((unit-test-buffer (unit-test-reset-buffer)))
       (save-window-excursion
         (pop-to-buffer unit-test-buffer)
         unit-test)
       (bury-buffer unit-test-buffer))))
  "A list of clauses that are added automatically to every unit test suite.")

(defadvice debug (before unit-test-core activate)
  "Automatically display the unit test buffer on entering the debugger."
  (when unit-test-buffer
    (pop-to-buffer unit-test-buffer)))

(defun unit-test-edebug-setup-hook ()
  "Dump accumulated error messaged on entering debugging."
  (when unit-test-running
    (let ((unit-test-running nil))
      (dolist (message (reverse unit-test-messages))
        (message "%s" message)))))

(defun unit-test-suite* (name &rest clauses)
  (require 'edebug)

  (let ((unit-test-suite-name name)
        (num-passed 0)
        (num-failed 0)
        (nontest-clauses unit-test-default-clauses)
        test-clauses)
    (add-to-list 'edebug-setup-hook
                 'unit-test-edebug-setup-hook)
    (dolist (clause clauses)
      (assert (listp clause))
      (assert (symbolp (car clause)))
      (if (or (string-match-p "^test-" (symbol-name (car clause)))
              (eq 'test (car clause)))
          (push clause test-clauses)
        (push clause nontest-clauses)))
    (dolist (test-clause (reverse test-clauses))

      (let* ((test-name (car test-clause))
             (run-count 0)
             (test-expr `(let () ,@(cdr test-clause))))
        (dolist (nontest-clause nontest-clauses)
          (setq test-expr
                (case (car nontest-clause)
                  ((let let* flet)
                   `(,@nontest-clause ,test-expr))
                  (set-up
                   `(progn ,@(cdr nontest-clause) ,test-expr))
                  (tear-down
                   `(unwind-protect ,test-expr ,@(cdr nontest-clause)))
                  (around
                   (subst `(progn (incf run-count) ,test-expr)
                          'unit-test
                          `(let ((run-count 0))
                             ,@(cdr nontest-clause)
                             (assert-equal 1 run-count))))
                  (t
                   (error "Uknown clause type: %S" (car nontest-clause))))))
        (with-function-name test-name
          (let ((debug-on-error (or debug-on-error
                                    (called-from-eval-defun-p))))
            (condition-case error
                (progn
                  (unit-test-started test-name)
                  (let ((unit-test-running t)
                        unit-test-messages)
                    (eval test-expr))
                  (unit-test-passed test-name)
                  (incf num-passed))
              ((debug error)
               (unit-test-failed test-name error)
               (incf num-failed)))))))
    (callf + unit-test-num-passed num-passed)
    (callf + unit-test-num-failed num-failed)
    (unit-test-message "%s: %s tests passesd, %d failed"
                       name num-passed num-failed)))

(defun unit-test-started (test-name)
  ;;(unit-test-message "%s:%s:" unit-test-suite-name test-name)
  )

(defun unit-test-passed (test-name))

(defun unit-test-failed (test-name error-info)
  (if (eq 'error (car-safe error-info))
      (unit-test-message "%s: FAILED: %s" test-name (cadr error-info))
    (unit-test-message "%s: FAILED: %S" test-name error-info)))

(defun unit-test-error (format &rest args)
  (apply 'error format args))

(defmacro* unit-test-suite (name &rest clauses)
  (declare (indent 1))
  `(with-function-name ',name
     (unit-test-suite*
      ',name
      ,@(mapcar (lambda (clause) `',clause) clauses))))

(defmacro expect-error (error-type &rest body)
  (declare (indent 1))
  (let ((error-sym (gensym "error")))
    `(when (condition-case ,error-sym
               (progn ,@body t)
             (,error-type nil)
             (error (unit-test-errror "expected error of type %S, caught %S" ',error-type ,error-sym)))
       (unit-test-error "expected error of type %S, caught no error" ',error-type))))

(defun unit-test-format-value (value expr)
  (let ((value-str (format "%S" value))
        (expr-str (format "%S" expr)))
    (if (or (equal value-str expr-str)
            (equal (concat "(quote " value-str ")") expr-str))
        value-str
      (format "%s (from %s)" value-str expr-str))))

(defun assert-pred* (pred expected-value expected-expr
                          found-value found-expr)
  (unless (funcall pred expected-value found-value)
    (unit-test-error
     (format "expected %s, got %s"
             (unit-test-format-value expected-value expected-expr)
             (unit-test-format-value found-value found-expr)))))

(defmacro assert-pred (pred expected-expr found-expr)
  `(assert-pred* ',pred
                 ,expected-expr ',expected-expr
                 ,found-expr ',found-expr))

(defmacro assert-not (found)
  `(assert-pred eq nil ,found))

(defalias 'assert-null 'assert-not)
(defalias 'assert-nil 'assert-not)

(defmacro assert-eq (expected found)
  `(assert-pred eq ,expected ,found))

(defmacro assert-eql (expected found)
  `(assert-pred eql ,expected ,found))

(defmacro assert-equal (expected found)
  `(assert-pred equal ,expected ,found))

(defmacro assert-member (expected-item found-list)
  `(assert-pred member ,expected-item ,found-list))

(defmacro assert-memq (expected-item found-list)
  `(assert-pred memq ,expected-item ,found-list))

(defmacro assert-looking-at (pattern)
  `(assert (looking-at ,pattern)))

(defmacro assert-looking-back (pattern)
  `(assert (looking-back ,pattern)))

(defmacro assert-invariant-under (predicate expr &rest body)
  (declare (indent 2))
  (if (eq 'list (car-safe expr))
      (if (null (cddr expr))
          nil
        `(assert-invariant-under ,predicate ,(cadr expr)
           (assert-invariant-under ,predicate (list ,@(cddr expr))
             ,@body)))
    (let ((old-value-sym (gensym "old-value"))
          (new-value-sym (gensym "new-value")))
      `(let ((,old-value-sym ,expr))
         (unwind-protect
             (progn ,@body)
           (let ((,new-value-sym ,expr))
             (unless (funcall ,predicate ,old-value-sym ,new-value-sym)
               (error "Value of %S changed from %S to %S"
                      ',expr ,old-value-sym ,new-value-sym))))))))

(defmacro assert-eq-invariant (expr &rest body)
  (declare (indent 1))
  `(assert-invariant-under 'eq ,expr ,body))

(defmacro assert-eql-invariant (expr &rest body)
  (declare (indent 1))
  `(assert-invariant-under 'eql ,expr ,body))

(defmacro assert-invariant (expr &rest body)
  (declare (indent 1))
  `(assert-invariant-under 'equal ,expr ,body))

(defmacro assert-default-invariants (&rest body)
  (declare (indent 0))
  `(progn
     (save-match-data)
     (assert-invariant (list (point)
                             (mark)
                             (match-data))
       (assert-eq-invariant (list (current-buffer)
                                  (current-window))
         ,@body))))

(provide 'unit-test-core)

;; Local Variables:
;; no-byte-compile: t
;; End:
