;; -*- no-byte-compile: t -*-
(require 'cl)

(defmacro regexp-case (key &rest clauses)
  "Macro for matching a string against multiple regular
  expressions.  This macro works just like `case' except that is
  uses `string-match' instead of `eqv'."
  (declare (indent 1))
  (let* ((key-var (make-symbol "key"))
         (cases (mapcar (function*
                         (lambda ((expr code))
                           (if (eq expr t)
                               `(t ,code)
                             `((string-match ,expr ,key-var) ,code))))
                        clauses)))
    `(let ((,key-var ,key))
       (cond ,@cases))))

(defun string-join (strings &optional separator)
  "Join STRINGS together into a single string, inserting SEPARATOR between every two strings.
If SEPARATOR is nil, uses \" \" as the separator."
  (let ((result ""))
    (while strings
      (if (cdr strings)
          (callf concat result (car strings) (or separator " "))
        (callf concat result (car strings)))
      (pop strings))
    result))

;; (unless (fboundp 'string-prefix-p)
;;   (defun string-prefix-p (prefix string)
;;     "Test whether STRING starts with PREFIX."
;;     (and (<= (length prefix) (length string))
;;          (string= prefix (substring string 0 (length prefix))))))

(defun string-suffix-p (suffix string)
  "Test whether STRING ends with SUFFIX."
  (and (<= (length suffix) (length string))
       (string= suffix (substring string
                                  (- (length string) (length suffix))))))

(defun string-with-prefix (prefix string)
  (if (string-prefix-p prefix string)
      string
    (concat prefix string)))

(defun string-with-suffix (suffix string)
  (if (string-suffix-p suffix string)
      string
    (concat string suffix)))

(defun string-without-prefix (prefix string)
  (if (string-prefix-p prefix string)
      (substring string (length prefix))
    string))

(defun string-without-suffix (suffix string)
  (if (string-suffix-p suffix string)
      (substring string 0 (- (length string) (length suffix)))
    string))

(defun string-without-prefix-re (prefix-re string)
  (let ((case-fold-search nil))
    (replace-regexp-in-string (string-with-prefix "^" prefix-re) "" string t)))

(defun string-without-suffix-re (suffix-re string)
  (let ((case-fold-search nil))
    (replace-regexp-in-string (string-with-suffix "$" suffix-re) "" string t)))

(defun regexp-quote* (&rest args)
  "Returns a regex that matches any string in ARGS."
  (concat "\\(?:" (string-join (mapcar 'regexp-quote args) "\\|") "\\)"))

(provide 'string-util)

;; Local Variables:
;; no-byte-compile: t
;; End:
