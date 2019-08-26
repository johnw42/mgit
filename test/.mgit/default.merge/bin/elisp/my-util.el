;; -*- no-byte-compile: t -*-
;; -*- byte-compile-warnings: (not cl-functions); -*-
(require 'cl-lib)
(require 'if-match)
(require 'jasmel)

(message "Loading my-util")


;; (defmacro with-macro-args (args form)
;;   (declare (indent 1))
;;   (if (null args)
;;       form
;;     (let* ((arg (car args))
;;            (new-var (gensym (symbol-name arg))))
;;       ``(let ,,`(list (list ',new-var ,arg))
;;           ,,`(let ((,arg ',new-var))
;;                (with-macro-args ,(cdr args) ,form))))))

(defmacro with-gensyms (symbol-list &rest body)
  "Evaluates BODY with each symbol in SYMBOL-LIST bound to a
value returned by `gensym'."
  (declare (indent 1))
  `(let ,(mapcar
          (lambda (symbol)
            `(,symbol (gensym ,(symbol-name symbol))))
          symbol-list)
     ,@body))

(jasmel-spec "with-gensyms"
  (jasmel-case "works"
    (jasmel-expect
        (macroexpand '(with-gensyms () body))
      :equal '(let () body)))
  (jasmel-expect
      (macroexpand '(with-gensyms (a) body))
    :equal '(let ((a (gensym "a"))) body))
  (jasmel-expect
      (macroexpand '(with-gensyms (a b) body1 body2))
    :equal '(let ((a (gensym "a")) (b (gensym "b"))) body1 body2)))

(defmacro pop-if-eq (value list)
  "Conditionally remove a value from a list.  If (eq VALUE (car
LIST)), calls (pop LIST) and returns t.  Otherwise leaves LIST
alone and returns nil."
  (with-gensyms (list-var)
    `(let ((,list-var ,list))
       (when (and ,list-var (eq ,value (car ,list-var)))
         (pop ,list)
         t))))

;; (defmacro safe-while (max-iterations condition &rest body)
;;   (declare (indent 2))
;;   (with-gensyms (i)
;;     `(let ((,i 0))
;;        (while ,condition
;;          (when (>= ,i ,max-iterations)
;;            (error "Too many iterations!"))
;;          (incf ,i)
;;          ,@body))))

(defun chain (&rest funs)
  "Construct a function by composing a series of functions:

 (chain foo bar baz) => (lambda (x) (foo (bar (baz x))))
 (chain foo) => 'foo
 (chain) => 'identity"
  (require 'cl)
  (if (null funs)
      'identity
    (let* ((args-sym (gensym "args"))
           (funs (reverse funs))
           (body `(apply ',(car funs) ,args-sym)))
      (dolist (fun (cdr funs))
        (setq body `(funcall ',fun ,body)))
      `(lambda (&rest ,args-sym)
         ,body))))

(jasmel-spec "chain"
  (jasmel-case "works"
    (jasmel-expect (funcall (chain) 42) :equal 42)
    (jasmel-expect (funcall (chain '1+) 1) :equal 2)
    (jasmel-expect (funcall (chain 'list '1+) 1) :equal '(2))
    (jasmel-expect (let ((foo 'list)
                         (bar '1+))
                     (funcall (chain foo bar) 1))
      :equal '(2))
    (jasmel-expect (funcall (chain (lambda (x) (list x))
                                   (lambda (x) (1+ x)))
                            1)
      :equal '(2))
    (jasmel-expect (funcall (chain (identity 'list)
                                   (identity '1+))
                            1)
      :equal '(2))))

(defmacro and-let* (clauses &rest body)
  "Implemention of SRFI-2.

Like an ordinary `and', an `and-let*' special form evaluates its arguments -- expressions -- one after another in order, till the first one that yields nil. Unlike `and', however, a non-nil result of one expression can be bound to a fresh variable and used in the subsequent expressions. The `and-let*' macro is a cross-breed between `let*' and `and'."
  (declare (indent 1)
           (debug ((&rest (sexp form)) body)))
  (require 'cl)
  (if (null clauses) `(progn ,@body)
    (let ((clause (car clauses))
          (tail `(and-let* (,@(cdr clauses)) ,@body)))
      (if (symbolp clause) `(and ,clause ,tail)
        (assert (listp clause))
        (if (= 1 (length clause)) `(and ,(first clause) ,tail)
          (if (= 2 (length clause))
              (let ((var (first clause))
                    (init (second clause)))
                `(let ((,var ,init)) (and ,var ,tail)))
            (error "Invalid clause: %S" clause)))))))

(jasmel-spec "and-let*"
  (jasmel-case "works"
    (jasmel-expect (and-let* () 'foo) :eq 'foo)
    (jasmel-expect (and-let* (t) 'foo) :eq 'foo)
    (jasmel-expect (and-let* (((not nil))) 'foo) :eq 'foo)
    (jasmel-expect (and-let* ((x 'foo)) x) :eq 'foo)
    (jasmel-expect :not (and-let* (nil) t))
    (jasmel-expect :not (and-let* ((x (not t))) t))
    (jasmel-expect :not (and-let* (((not t))) t))))

(defun split-into (symbol-list value-list)
  "Analogous to `set' with list splitting.

Assigns values in VALUE-LIST to the corresponding symbols in
SYMBOL-LIST, which may be improper.  If SYMBOL-LIST is improper
then VALUE-LIST may also be improper.  Assinging to nil is a
no-op."
  (require 'cl)
  (while (and symbol-list value-list)
    (let (symbol value)
      (if (symbolp symbol-list)
          (setq symbol symbol-list
                value value-list
                symbol-list nil
                value-list nil)
        (setq symbol (pop symbol-list)
              value (pop value-list)))
      (unless (null symbol)
        (set symbol value))))
  (if symbol-list
      (error "Not enough values"))
  (if value-list
      (error "Too many values")))

(defmacro splitq (symbols values)
  "Like `split-into', but SYMBOLS is automatically quoted."
  (require 'cl)
  `(split-into ',symbols ,values))

(defmacro split-let* (bindings &rest body)
  "Like `let*', but the fist form of each binding may be a list
of symbols instead of a single symbol, and the symbols are
assigned using `split'."
  (declare (indent 1))
  (require 'cl)
  (if (null bindings) `(progn ,@body)
    (let* ((binding (pop bindings)))
      (if (or (symbolp binding)
              (symbolp (car binding)))
          `(let (,binding) (split-let* (,@bindings) ,@body))
        (let ((unprocessed-symbols (first bindings))
              symbols-to-bind)
          (while unprocessed-symbols
            (push (if (symbolp unprocessed-symbols)
                      (prog1 unprocessed-symbols
                        (setq unprocessed-symbols nil))
                    (pop unprocessed-symbols))
                  symbols-to-bind))
          (setq symbols-to-bind
                (remove-if 'null symbols-to-bind))
          `(let (,@symbols-to-bind)
             (splitq ,@binding)
             (split-let* (,@bindings)
               ,@body)))))))

;; (defun add-or-remove-hook (addp hook function &optional append local)
;;   "Call `add-hook' if ADDP is non-nil, else call `remove-hook'."
;;   (if addp
;;       (add-hook hook function append local)
;;     (remove-hook hook function local)))

;; (defmacro* dolines ((&optional start end)
;;                     &rest body)
;;   "Execute BODY for each line of the current buffer."
;;   (declare (indent 1))
;;   (with-macro-vars (end-var)
;;     `(save-excursion
;;        ,(if start
;; 	    `(goto-char ,start)
;; 	  `(goto-char (point-min)))
;;        (let ((,end-var ,end))
;; 	 (or (bolp) (forward-line))
;; 	 (while (and (< (point) ,end-var)
;; 		     (progn
;; 		       (save-excursion ,@body)
;; 		       (forward-line)
;; 		       (not (eobp)))))))))

(defsubst mklist (item)
  "Wrap ITEM in a list if it is not aready a list."
  (if (listp item) item (list item)))

(defmacro* point-after (&body body)
  "Return the location where the point ends up after executing
BODY.  The location of the point is not changed."
  `(save-excursion ,@body (point)))

(defalias 'with-macro-vars 'with-gensyms)

(defun string-replace (replace-in to-replace replace-with)
  "DEPRECATED.  Use replace-regexp-in-string instead."
  (save-match-data
    (replace-regexp-in-string (regexp-quote to-replace)
                              (regexp-quote replace-with)
                              replace-in)))

(defun read-symbol (prompt
                    table
                    &optional
                    predicate
                    require-match
                    initial-input
                    hist
                    def)
  "Read a symbol from the minibuffer."
  (intern
   (completing-read prompt
                    (cond
                     ((listp table)
                      (mapcar 'symbol-name table))
                     (t table))
                    predicate
                    require-match
                    initial-input
                    hist
                    def)))

(defun iso-format-date (&optional time)
  "Format a timestamp in the form returned by `current-time' as
an ISO-8859 date."
  (format-time-string "%Y-%m-%d" time))

(defun maybe-call (func &rest args)
  "Similar to `apply' except that if any argument is `nil',
returns `nil' without calling FUNC."
  (catch 'escape
    (dolist (arg (cons func args) (apply func args))
      (if (not arg)
          (throw 'escape nil)))))

(defun sort-alist (list key-predicate &optional value-predicate)
  "Sort a association list by keys and optionally by values."
  (sort list
        (lambda (a b)
          (or (funcall key-predicate (car a) (car b))
              (and value-predicate
                   (not (funcall key-predicate (car b) (car a)))
                   (funcall value-predicate (cdr a) (cdr b)))))))

(defun filename-split (path)
  "Convert a filename into a list of directory components."
  (let ((basename (file-name-nondirectory path))
        (dirname (directory-file-name (file-name-directory path))))
    (if (string= dirname path)
        ()
      (append (filename-split dirname)
              (list basename)))))

(defun filename-join (parts)
  "The inverse of `filename-split'."
  (if (null parts)
      "/"
    (let ((basename (car (last parts)))
          (dirname (file-name-as-directory (filename-join (butlast parts)))))
        (concat dirname basename))))

;; (defun my-filename= (n1 n2)
;;   (equal (expand-file-name n1)
;;          (expand-file-name n2)))

;; (defun fix (p f x)
;;   "Fixed-point finder.
;; Repeatedly apply unary function f to argument x.  Return the first value of x
;; such that (p x (f x)) is true."
;;   (car (last (my-scanfix p f x))))

(defun take-while (pred list)
  "Return the longest prefix of LIST for while PRED retuns non-nil for every item."
  (let (result)
    (while (funcall pred (car list))
      (push (pop list) result))
    (nreverse result)))

(defsubst take-until (pred list)
  "Return the longest prefix of LIST for while PRED retuns nil for every item."
  (take-while (chain #'not pred) list))

(defun drop-while (pred list)
  "Return the longest suffix of LIST for which PRED returns nil for the first element."
  (while (and list (funcall pred (car list)))
    (pop list))
  list)

(defsubst drop-until (pred list)
  "Return the longest suffix of LIST for which PRED returns non-nil for the first element."
  (drop-while (chain #'not pred) list))

(defun map-file-lines (fn filename)
  "Calls FN for every line in the file at FILENAME.
The function is passed one argument, the line (not including a
newline character."
  (with-lexical-vars (fn)
    (with-temp-buffer
      (insert-file-contents filename)
      (map-buffer-lines fn))))

(defun map-buffer-lines (fn &optional buffer)
  "Calls FN for every line in BUFFER (default: current buffer).
The function is passed one argument, the line (not including a
newline character.  Before each call to FN, the point is moved to
the beginning of the next line in the buffer."
  (if buffer
      (with-current-buffer buffer (map-buffer-lines fn))
    (with-lexical-vars (fn)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line-start (point))
                  (line-end (progn (end-of-line) (point))))
              (goto-char line-start)
              (save-excursion
                (funcall fn (buffer-substring-no-properties line-start line-end)))
              (goto-char line-end)
              (unless (eobp) (forward-char)))))))))

(defun buffer-lines (&optional buffer)
  "Get a list containing all the lines in the current buffer."
  (map-to-list 'map-buffer-lines buffer))

(defun buffer-lines-with-point (&optional buffer)
  "Get a list of pairs, one for each line in BUFFER.  Each pair's `cdr' is the
starting point of the line, and its `car' is the line itself."
  (with-list-result r
    (map-buffer-lines
     (lambda (line)
       (push (cons line (point)) r))
     buffer)))

(defun map-forward-matches-from-point (fn regex &optional limit)
  (with-lexical-vars (fn)
    (save-match-data
      (save-excursion
        (while
            (let ((pos (re-search-forward regex limit t)))
              (when pos
                (funcall fn pos)
                t)))))))

(defun map-backward-matches-from-point (fn regex &optional limit)
  (with-lexical-vars (fn)
    (save-match-data
      (save-excursion
        (while
            (let ((pos (re-search-backward regex limit t)))
              (when pos
                (funcall fn pos)
                t)))))))

(defun map-buffer-matches (fn regex &optional buffer)
  "Calls FN for every match of REGEX in BUFFER (default: current buffer).
The function is passed one argument, the position of the start of
the match. The point will be at at end of the match."
  (if buffer
      (with-current-buffer buffer (map-buffer-matches regex fn))
    (with-lexical-vars (fn)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (map-forward-matches-from-point fn regex))))))

(defun map-to-list (map-function &rest remaining-args)
  "Calls MAP-FUNCTION, passing a dummy function and
REMAINING-ARGS, and returns a list of the arguments that are
passed to the dummy function.  Useful for functions like `mapc'."
  (let (accum)
    (apply map-function
           (lambda (arg) (push arg accum))
           remaining-args)
    (nreverse accum)))

(defun map-ancestor-dirs (fn &optional dir-or-file-or-buffer)
  (with-lexical-vars (fn)
    (let ((dir (cond
                ((null dir-or-file-or-buffer)
                 default-directory)
                ((bufferp dir-or-file-or-buffer)
                 (buffer-local-value 'default-directory dir-or-file-or-buffer))
                ((file-directory-p dir-or-file-or-buffer)
                 dir-or-file-or-buffer)
                ((file-directory-p (file-name-directory dir-or-file-or-buffer))
                 (file-name-directory dir-or-file-or-buffer))
                (t
                 (error "invalid argument: %S" dir-or-file-or-buffer)))))
      (assert (file-name-absolute-p dir))
      (while
          (when dir
            (let ((parent-dir (file-name-directory (directory-file-name dir))))
              (funcall fn dir)
              (and (not (equal dir parent-dir))
                   (setq dir parent-dir))))))))

(defun ancestor-dirs (&optional dir-or-file-or-buffer)
  (map-to-list 'map-ancestor-dirs dir-or-file-or-buffer))

(defun find-in-ancestor-dirs (name &optional predicate)
  "Searchs in the ancestors of `default-directory' for a file
named NAME that matches PREDICATE.  When PREDICATE is specified,
it is called with the full path of the candidate file as the
argument.  Returns the the path of the file if found."
  (assert (stringp name))
  (catch 'found
    (dolist (dir (ancestor-dirs))
      (let ((path (concat dir name)))
       (when (and (file-exists-p path)
                  (or (not predicate)
                      (funcall predicate path)))
         (throw 'found path))))))

;; (defun* make-test-func (&rest args &key test test-not)
;;   (when (and test test-not)
;;     (error "Either test or test-not must be nil."))
;;   (cond
;;    (test test)
;;    (test-not (chain 'not test-not))
;;    (t 'identity)))

(defun* group-by (func items &key test)
  (let ((table (make-hash-table :size (length items)
                                :test (or test 'eql)))
        result)
    (dolist (item items)
      (let ((key (funcall func item)))
        (puthash key
                 (cons item
                       (gethash key table))
                 table)))
    (maphash
     (lambda (key value)
       (push (cons key (nreverse value)) result))
     table)
    result))

(defalias 'if-string-match 'if-match)
(defalias 'when-string-match 'when-match)

(defmacro looking-back-p (&rest args)
  "Like `looking-back', but does not alter the global match data."
  `(save-match-data (looking-back ,@args)))

(defmacro with-list-result (var-name &rest body)
  "Declares VAR-NAME in BODY and returns (nreverse VAR-NAME)."
  (declare (indent 1))
  `(let (,var-name)
     ,@body
     (nreverse ,var-name)))

(defmacro with-lexical-vars (vars &rest body)
  "Evaluated BODY with each variable named in VARS copied to a
lexically-scoped variable."
  (declare (indent 1))
  ;; (if (not vars)
  ;;     `(progn ,@body)
  ;;   (let* ((old-var (car vars))
  ;;          (new-var (gensym (symbol-name old-var))))
  ;;     `(let ((,new-var ,old-var))
  ;;        (with-lexical-vars ,(cdr vars)
  ;;          ,@(subst
  ;;             new-var
  ;;             old-var
  ;;             body)))))
  `(lexical-let ,(mapcar (lambda (x) `(,x ,x)) vars)
     ,@body))

(defun insert-with-markers (&rest args)
  "Insert text and update point, mark or an arbitrary marker to
point to a specific location in the inserted text."
  (let (final-point)
    (dolist (arg args)
      (cond
       ((stringp arg)
        (insert arg))
       ((markerp arg)
        (set-marker arg (point)))
       ((eq arg 'point)
        (setq final-point (point)))
       ((eq arg 'mark)
        (set-mark (point)))))
    (when final-point
      (goto-char final-point))))

(defun yank-with-replace (pattern replacements)
  (dolist (replacement replacements)
    (insert (replace-regexp-in-string
             pattern
             replacement
             (current-kill 0)))))

(defun remove-from-list (list-var element &optional _append compare-fn)
  (set list-var (remove* element
                         (symbol-value list-var)
                         :test (or compare-fn 'equal))))

(provide 'my-util)

;; Local Variables:
;; no-byte-compile: t
;; End:
