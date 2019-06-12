;; -*- no-byte-compile: t -*-
(require 'cl)
(require 'jasmel)

(message "Loading alist")

(defun funcall-sexp (func &rest args)
  "Generates either a function call expression or a call to
funcall."
  (if (symbolp func)
      `(,func ,@args)
    `(funcall ',func ,@args)))

(jasmel-spec "funcall-sexp"
  (jasmel-case "converts calls to symbols to normal function calls"
    (jasmel-expect (funcall-sexp 'foo 1 2) :equal '(foo 1 2)))

  (jasmel-case "converts calls to non-symbols to use funcall"
    (jasmel-expect (funcall-sexp '(expr) 1 2) :equal '(funcall '(expr) 1 2))))

(defun compose (&rest funcs)
  "Composes zero or more functions together.  Treats nil as equal
to 'identity."
  (let* ((funcs (nreverse (delq nil (remq 'identity funcs))))
         (num-funcs (length funcs)))
    (case num-funcs
      (0 'identity)
      (1 (car funcs))
      (t
       (let* ((args-sym (gensym "fcompose-args"))
              (body `(apply ',(car funcs) ,args-sym)))
         (dolist (func (cdr funcs)
                       `(lambda (&rest ,args-sym) ,body))
           (when (not (eq 'identity func))
             (setq body (funcall-sexp func body)))))))))

(jasmel-spec "compose"

  (jasmel-case "returns the identity function when called with no arguments"
    (jasmel-expect (compose) :eq 'identity))
  ;;(expect 'identity (compose))

  (jasmel-case "returns a single arguemnt unchanged"
    (jasmel-expect (compose '1+) :eq '1+))

  (jasmel-case "can compose two functions"
    (jasmel-expect (funcall (compose 'list '1+) 1) :equal '(2)))

  (jasmel-case "ignores nil arguments"
    (jasmel-expect (funcall (compose nil 'list nil '1+ nil) 1) :equal '(2)))

  (jasmel-case "works when arguments are variables"
    (jasmel-expect (let ((foo 'list)
                         (bar '1+))
                     (funcall (compose foo bar) 1))
      :equal '(2)))

  (jasmel-case "works when arguments are lambda expressions"
    (jasmel-expect (funcall (compose (lambda (x) (list x))
                                     (lambda (x) (1+ x)))
                            1) :equal '(2)))

  (jasmel-case "works when arguments are complex expressions"
    (jasmel-expect (funcall (compose (identity 'list)
                                     (identity '1+))
                            1) :equal '(2))))

(defun partial (func &rest bound-args)
  "Partially apply a function."
  (if (null bound-args)
      func
    (let ((args-sym (gensym "partial-args"))
          (quoted-args (mapcar (lambda (arg) `',arg) bound-args)))
      `(lambda (&rest ,args-sym) (apply ',func ,@quoted-args ,args-sym)))))

(jasmel-spec "partial"

  (jasmel-case "produces a function that accepts more arguments"
    (jasmel-expect (funcall (partial 'list) 1 2) :equal '(1 2)))

  (jasmel-case "binds arguments to a function"
    (jasmel-expect (funcall (partial 'list 1 2)) :equal '(1 2)))

  (jasmel-case "puts bound arguments are before later arguemnts"
    (jasmel-expect (funcall (partial 'list 1 2) 3 4) :equal '(1 2 3 4)))

  (jasmel-case "binds expressions by value"
    (jasmel-expect (funcall (let ((a 1) (b 2))
                              (partial 'list a b)) 3 4)
      :equal '(1 2 3 4))))

(defun* alist-put (alist item-car item-cdr &key key test test-not)
  "Destructively inserts (car ITEM-CAR ITEM-CDR) into an alist,
replacing any existing items whose car equals ITEM-CAR."
  (cons (cons item-car item-cdr)
        (alist-delete alist item-car :key key :test test :test-not test-not)))

(jasmel-spec "alist-put"

  (jasmel-case "adds a new item and removes the old item"
    (jasmel-expect (alist-put '((a . 1) (b . 2) (c . 3)) 'c 4)
      :equal '((c . 4) (a . 1) (b . 2)))))

(defun* alist-get (alist item-car &key key test test-not)
  "Finds and returns the `cdr' of the item in ALIST whose `car'
is ITEM-CAR.  Returns nil if there is no such item."
  (cdr (assoc* item-car alist :key key :test test :test-not test-not)))

(jasmel-spec "alist-get"

  (jasmel-case "can find an existing item"
    (jasmel-expect (alist-get '((a . 1) (b . 2) (c . 3)) 'b)
      :equal '2)))

(defun* alist-member (alist item-car &key key test test-not)
  "Identical to `assoc*' but with a different argument order."
  (assoc* item-car alist :key key :test test :test-not test-not))

(jasmel-spec "alist-member"
  (jasmel-case "can find an entry"
    (jasmel-expect (alist-member '((a . 1) (b . 2) (c . 3)) 'b)
      :equal '(b . 2))))

(defun* alist-remove (alist item-car &key key test test-not)
  "Returns a copy of ALIST when any pair whose `car' is ITEM-CAR
is removed."
  (remove* item-car alist :key (compose key 'car) :test test :test-not test-not))

(jasmel-spec "alist-remove"
  (jasmel-case "can remove an entry"
    (jasmel-expect (alist-remove '((a . 1) (b . 2) (c . 3)) 'b)
      :equal '((a . 1) (c . 3)))))

(defun* alist-delete (alist item-car &key key test test-not)
  "Destructive variant of `alist-remove'."
  (delete* item-car alist :key (compose key 'car) :test test :test-not test-not))

(jasmel-spec "alist-delete"
  (jasmel-case "can remove an entry"
    (jasmel-expect (alist-delete '((a . 1) (b . 2) (c . 3)) 'b)
      :equal '((a . 1) (c . 3)))))

(provide 'alist)

;; Local Variables:
;; no-byte-compile: t
;; End:
