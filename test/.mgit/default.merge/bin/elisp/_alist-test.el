;; -*- no-byte-compile: t -*-
(require 'alist)
(require 'unit-test-core)

(unit-test-suite alist-test
  (test-compose
   (assert-eq 42 (funcall (compose) 42))
   (assert-equal 2 (funcall (compose '1+) 1))
   (assert-equal '(2) (funcall (compose 'list '1+) 1))
   (assert-equal '(2) (funcall (compose nil 'list nil '1+ nil) 1))
   (assert-equal '(2)
                 (let ((foo 'list)
                       (bar '1+))
                   (funcall (compose foo bar) 1)))
   (assert-equal '(2) (funcall (compose (lambda (x) (list x))
                                        (lambda (x) (1+ x)))
                               1))
   (assert-equal '(2) (funcall (compose (identity 'list)
                                        (identity '1+))
                               1)))

  (test-funcall-sexp
   (assert-equal '(foo 1 2) (funcall-sexp 'foo 1 2))
   (assert-equal '(funcall '(expr) 1 2) (funcall-sexp '(expr) 1 2)))

  (test-partial
   (assert-equal '(1 2) (funcall (partial 'list) 1 2))
   (assert-equal '(1 2) (funcall (partial 'list 1 2)))
   (assert-equal '(1 2 3 4) (funcall (partial 'list 1 2) 3 4))
   (assert-equal '(1 2 3 4) (funcall (let ((a 1) (b 2))
                                       (partial 'list a b)) 3 4)))

  (test-alist-put
   (assert-equal '((c . 4) (a . 1) (b . 2))
                 (alist-put '((a . 1) (b . 2) (c . 3)) 'c 4)))

  (test-alist-get
   (assert-equal '2
                 (alist-get '((a . 1) (b . 2) (c . 3)) 'b)))

  (test-alist-member
   (assert-equal '(b . 2)
                 (alist-member '((a . 1) (b . 2) (c . 3)) 'b)))

  (test-alist-remove
   (assert-equal '((a . 1) (c . 3))
                 (alist-remove '((a . 1) (b . 2) (c . 3)) 'b)))

  (test-alist-delete
   (assert-equal '((a . 1) (c . 3))
                 (alist-delete '((a . 1) (b . 2) (c . 3)) 'b))))

;; Local Variables:
;; no-byte-compile: t
;; End:
