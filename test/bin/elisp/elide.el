;; -*- no-byte-compile: t -*-
;; -*- lexical-binding: t; -*-
(require 'jasmel)
;;(elide/ns elide)

(message "Loading elide")

(defvar elide/namespace-name nil)
(defvar elide/namespace-prefix nil)
(put 'elide/namespace-prefix
     'safe-local-variable
     'stringp)

(defvar elide/namespaces (make-hash-table :test 'equal))

(defmacro elide/ns (_namespace &rest _rest)
  (declare (indent 1)))

(defun elide/safe-intern (symbol-or-string)
  (if (symbolp symbol-or-string)
      symbol-or-string
    (intern symbol-or-string)))

(jasmel-spec "elide/safe-intern"
  (jasmel-case "returns a symbol unchanged"
    (let ((sym (make-symbol "dummy-symbol")))
      (jasmel-expect (elide/safe-intern sym) :eq sym)))
  (jasmel-case "interns a string"
    (jasmel-expect (elide/safe-intern "foo") :eq 'foo))
  (jasmel-case "is nil-safe"
    (jasmel-expect (elide/safe-intern nil) :eq nil)))

(defun elide/safe-symbol-name (symbol-or-string)
  (if (stringp symbol-or-string)
      symbol-or-string
    (symbol-name symbol-or-string)))

(jasmel-spec "elide/safe-symbol-name"
  (jasmel-case "returns a string unchanged"
    (let ((str (concat "dummy-" "string")))
      (jasmel-expect (elide/safe-symbol-name str) :eq str)))
  (jasmel-case "gets a symbol's name"
    (jasmel-expect (elide/safe-symbol-name 'foo) :equal "foo"))
  (jasmel-case "handles nil"
    (jasmel-expect (elide/safe-symbol-name nil) :equal "nil")))

(defvar elide/decompose-symbol-cache
  (make-hash-table :test 'equal))

(defun elide/decompose-symbol (symbol-or-string)
  (or (gethash symbol-or-string elide/decompose-symbol-cache)
      (puthash symbol-or-string
               (let* ((name (elide/safe-symbol-name symbol-or-string)))
                 (if (string-match "\\([^/]+\\)/\\(.*\\)" name)
                     (cons (match-string 1 name)
                           (match-string 2 name))
                   (cons nil name)))
               elide/decompose-symbol-cache)))

(jasmel-spec "elide/decompose-symbol"

  (jasmel-before-each
    (clrhash elide/decompose-symbol-cache))

  (jasmel-case "uses a cached value"
    (puthash "foo/bar" "cached value" elide/decompose-symbol-cache)
    (jasmel-expect (elide/decompose-symbol "foo/bar")
      :equal "cached value"))

  (jasmel-case "caches values"
    (let ((value (elide/decompose-symbol "foo/bar")))
      (jasmel-expect (gethash "foo/bar" elide/decompose-symbol-cache)
        :equal value)))

  (jasmel-case "splits a qualified symbol"
    (jasmel-expect (elide/decompose-symbol 'foo/bar)
      :equal '("foo" . "bar")))

  (jasmel-case "splits a qualified symbol name"
    (jasmel-expect (elide/decompose-symbol "foo/bar")
      :equal '("foo" . "bar")))

  (jasmel-case "handles an unqualified symbol"
    (jasmel-expect (elide/decompose-symbol 'bar)
      :equal '(nil . "bar")))

  (jasmel-case "handles / itself"
    (jasmel-expect (elide/decompose-symbol '/)
      :equal '(nil . "/")))

  (jasmel-case "handles symbols starting with /"
    (jasmel-expect (elide/decompose-symbol '/=)
      :equal '(nil . "/=")))

  (jasmel-case "handles nil"
    (jasmel-expect (elide/decompose-symbol nil)
      :equal '(nil . "nil"))))

;; (defun elide/qualify-symbol (ns-or-symbol-or-string symbol-or-string)
;;   (let ((ns-name (if (elide/namespace-p ns-or-symbol-or-string)
;;                      (symbol-name
;;                       (elide/namespace-symbol ns-or-symbol-or-string))
;;                    (elide/safe-symbol-name ns-or-symbol-or-string)))
;;         (name (elide/safe-symbol-name symbol-or-string)))
;;     ()))

(defun elide/namespace (ns-or-symbol-or-string)
  (if (elide/namespace-p ns-or-symbol-or-string)
      ns-or-symbol-or-string
    (let ((name (elide/safe-symbol-name ns-or-symbol-or-string)))
      (assert (not (string-match-p ".*/.*" name)))
      (or (gethash name elide/namespaces)
          (puthash name
                   (elide/make-namespace
                    :name name
                    :names (make-hash-table :test 'equal))
                   elide/namespaces)))))

(defun elide/namespace-soft (ns-or-symbol-or-string)
  (if (elide/namespace-p ns-or-symbol-or-string)
      (elide/namespace-name ns-or-symbol-or-string)
    (gethash (elide/safe-symbol-name ns-or-symbol-or-string)
             elide/namespaces)))

;; (defun elide/safe-namespace-name (ns-or-symbol-or-string)
;;   (if (elide/namespace-p ns)
;;       (elide/namespace-name ns)
;;     (elide/safe-symbol-name ns)))


(defun elide/mapatoms-speed-test ()
  (mapatoms 'elide/dummy-func))

(defun elide/update-namespaces ()
  (mapatoms
   (lambda (sym)
     (let* ((pair (elide/decompose-symbol sym))
            (ns-name (car pair)))
       (when ns-name
         (let* ((member-name (cdr pair))
                (ns (elide/namespace ns-name))
                (ns-names (elide/namespace-names ns)))
           (puthash member-name sym ns-names)))
       ))))

;; (defun elide/intern (ns name)
;;   (setq ns (elide/namespace ns))
;;   (setq name (elide/safe-symbol-name name))
;;   (let ((ns-name (elide/namespace-name ns))
;;         (ns-names (elide/namespace-names ns)))
;;     (or (gethash name ns-names)
;;         (puthash name
;;                  (intern (concat ns-name "/" name))
;;                  ns-names))))

(defun elide/intern-soft (elide/ns name)
  (setq elide/ns (elide/namespace-soft elide/ns))
  (and elide/ns
       (gethash name
                (elide/namespace-names elide/ns))))

;; (defun elide/intern namespace symbol-or-string
;;   "Adds a symbol to a namespace and returns the emacs symbol."
;;   nil)

(defun elide/hash-keys (table)
  (and table
       (with-list-result keys
         (maphash (lambda (key _value)
                    (push key keys))
                  table))))

(jasmel-spec "elide/hash-keys"

  (jasmel-case "returns nil for nil"
    (jasmel-expect (elide/hash-keys nil) :eq nil))

  (jasmel-case "returns nil for an empty table"
    (let ((table (make-hash-table :test 'equal)))
      (jasmel-expect (elide/hash-keys table) :eq nil)))

  (jasmel-case "returns the keys of a table"
    (let ((table (make-hash-table :test 'equal)))
      (puthash "a" 1 table)
      (puthash "b" 1 table)
      (puthash "c" 1 table)
      (jasmel-expect (sort (elide/hash-keys table) 'string<)
        :equal '("a" "b" "c")))))

(defmacro elide/defstruct (symbol &rest slots)
  (declare (indent 1))
  (assert (symbolp symbol))
  (let* ((pair (elide/decompose-symbol symbol))
         (name (cdr pair))
         (prefix (if (car pair) (concat (car pair) "/") "")))
    `(defstruct (,(intern (concat prefix name))
                 (:constructor ,(intern (concat prefix "make-" name)))
                 (:copier ,(intern (concat prefix "copy-" name))))
       ,@slots)))

(jasmel-spec "elide/defstruct"
  (let (test-struct)

    (jasmel-before-each
      (elide/defstruct elide/test-struct a b)
      (setq test-struct (elide/make-test-struct :a 1 :b 2)))

    (jasmel-after-each
      (fmakunbound 'elide/test-struct-a)
      (fmakunbound 'elide/test-struct-b)
      (fmakunbound 'elide/test-struct-p)
      (fmakunbound 'elide/make-test-struct)
      (fmakunbound 'elide/copy-test-struct))

    (jasmel-case "has a type tag"
      (jasmel-expect test-struct :equal [cl-struct-elide/test-struct 1 2]))

    (jasmel-case "getters work"
      (jasmel-expect (elide/test-struct-a test-struct) :equal 1)
      (jasmel-expect (elide/test-struct-b test-struct) :equal 2))

    (jasmel-case "setters work"
      (incf (elide/test-struct-a test-struct))
      (incf (elide/test-struct-b test-struct))
      (jasmel-expect (elide/test-struct-a test-struct) :equal 2)
      (jasmel-expect (elide/test-struct-b test-struct) :equal 3))

    (jasmel-case "copier works"
      (let ((new-struct (elide/copy-test-struct test-struct)))
        (jasmel-expect (elide/test-struct-a new-struct) :equal 1)
        (jasmel-expect (elide/test-struct-b new-struct) :equal 2)))

    (jasmel-case "predicate works"
      (jasmel-expect (elide/test-struct-p test-struct) :equal t)
      (jasmel-expect :not (elide/test-struct-p t)))))

(elide/defstruct elide/namespace
  name names)

(provide 'elide)

;; Local Variables:
;; no-byte-compile: t
;; End:
