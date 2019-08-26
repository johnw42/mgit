;; -*- no-byte-compile: t -*-
(defvar sgs-undo-exprs)
(defvar sgs-modified-symbols)

(defconst sgs-orig-set (symbol-function 'set))
(defconst sgs-orig-setq (symbol-function 'setq))
(defconst sgs-orig-fset (symbol-function 'fset))
(defconst sgs-orig-intern (symbol-function 'intern))

(defun sgs-intern (symbol-name &optional obarray-arg)
  (when (and (not (intern-soft symbol-name))
             (or (null obarray-arg)
                 (eq obarray-arg obarray)))
    (push `(unintern ',symbol-name) sgs-undo-exprs))
  (funcall sgs-orig-intern symbol-name obarray-arg))

(defmacro sgs-setq (symbol value &rest rest)
  (if rest
      `(progn
         (sgs-set ',symbol ,value)
         (sgs-setq ,@rest))
    `(sgs-set ',symbol ,value)))

(defun sgs-set (symbol value)
  (unless (gethash symbol sgs-modified-symbols)
    (puthash symbol t sgs-modified-symbols)
    (funcall sgs-orig-set
             'sgs-undo-exprs
             (cons
              (if (boundp symbol)
                  `(set ',symbol ',(symbol-value symbol))
                `(makunbound ',symbol))
              sgs-undo-exprs)))
  (funcall sgs-orig-set symbol value))

(defun save-global-state* (excluded-symbols expr)
  (let ((sgs-undo-exprs nil)
        (sgs-modified-symbols (make-hash-table :test 'eq)))
    (unwind-protect
        (progn
          (fset 'intern (symbol-function 'sgs-intern))
          (fset 'set (symbol-function 'sgs-set))
          (fset 'setq (symbol-function 'sgs-setq))
          (assert (hash-table-p sgs-modified-symbols))
          (eval expr))
      (funcall sgs-orig-fset 'fset sgs-orig-fset)
      (fset 'set sgs-orig-set)
      (fset 'setq sgs-orig-setq)
      (fset 'intern sgs-orig-intern)
      (eval `(progn ,@sgs-undo-exprs)))))

(progn
  (defconst constant-symbols nil)
  (mapatoms (lambda (symbol)
              (condition-case err
                  (when (boundp symbol)
                    (set symbol (symbol-value symbol)))
                (setting-constant
                 (push symbol constant-symbols)))))
  constant-symbols)

(defmacro save-global-state (excluded-symbols &rest body)
  (declare (indent 1))
  `(save-global-state* ,excluded-symbols '(progn ,@body)))

(provide 'save-global-state)

;; Local Variables:
;; no-byte-compile: t
;; End:
