;; -*- no-byte-compile: t -*-
(require 'my-util)
(require 'unit-test-core)

(unit-test-suite my-util-test

  ;; (test-with-macro-args
  ;;  (defmacro test-macro (x)
  ;;    (with-macro-args ()
  ;;     `(list ,x ,x)))
  ;;  (let ((a 0))
  ;;    (assert-equal '(1 2) (test-macro-0 (incf a)))
  ;;    (assert-equal 2 a))

  ;;  (defmacro test-macro (x)
  ;;    (with-macro-args (x)
  ;;      `(list ,x ,x)))
  ;;  (let ((a 0))
  ;;    (assert-equal '(1 1) (test-macro (incf a)))
  ;;    (assert-equal 1 a))

  ;;  (defmacro test-macro (x y)
  ;;    (with-macro-args (x y)
  ;;      `(list ,x ,x ,y ,y)))
  ;;  (let ((a 1)
  ;;        (b -1))
  ;;    (assert-equal '(2 2 -2 -2) (test-macro (incf a) (decf b)))
  ;;    (assert-equal 2 a)
  ;;    (assert-equal -2 b)))

  (test-with-gensyms
   (assert-equal
    '(let () body)
    (macroexpand '(with-gensyms () body)))
   (assert-equal
    '(let ((a (gensym "a"))) body)
    (macroexpand '(with-gensyms (a) body)))
   (assert-equal
    '(let ((a (gensym "a")) (b (gensym "b"))) body1 body2)
    (macroexpand '(with-gensyms (a b) body1 body2))))

  (test-chain
   (assert-eq 42 (funcall (chain) 42))
   (assert-equal 2 (funcall (chain '1+) 1))
   (assert-equal '(2) (funcall (chain 'list '1+) 1))
   (assert-equal '(2)
                 (let ((foo 'list)
                       (bar '1+))
                   (funcall (chain foo bar) 1)))
   (assert-equal '(2) (funcall (chain (lambda (x) (list x))
                                      (lambda (x) (1+ x)))
                               1))
   (assert-equal '(2) (funcall (chain (identity 'list)
                                      (identity '1+))
                               1)))

  (test-and-let*
   (assert-eq 'foo (and-let* () 'foo))
   (assert-eq 'foo (and-let* (t) 'foo))
   (assert-eq 'foo (and-let* (((not nil))) 'foo))
   (assert-eq 'foo (and-let* ((x 'foo)) x))
   (assert-null (and-let* (nil) t))
   (assert-null (and-let* ((x (not t))) t))
   (assert-null (and-let* (((not t))) t)))

  (test-split-into
   (let (x y z)
     (split-into '(x y z) '(a b c))
     (assert-equal (list x y z) '(a b c)))
   (let (x y z)
     (split-into '(x y . z) '(a b c))
     (assert-equal (list x y z) '(a b (c))))
   (let (x y z)
     (split-into '(x y . z) '(a b . c))
     (assert-equal (list x y z) '(a b c))))

  (test-splitq
   (let (x y z)
     (splitq (x y z) '(a b c))
     (assert-equal '(a b c) (list x y z)))
   (let (x y z)
     (splitq (x y . z) '(a b c))
     (assert-equal '(a b (c)) (list x y z)))
   (let (x y z)
     (splitq (x y . z) '(a b . c))
     (assert-equal '(a b c) (list x y z))))

  (test-split-let*
   (split-let* ((abc '(a b c))
                ((x y z) abc))
     (assert-equal '(a b c) abc)
     (assert-equal '(a b c) (list x y z))))

  (test-string-replace
   (assert-equal "bar" (string-replace "foo" "foo" "bar"))
   (assert-equal "bar bar" (string-replace "foo bar" "foo" "bar"))
   (assert-equal "xbarx" (string-replace "xfoox" "foo" "bar"))
   (assert-equal "bar bar" (string-replace "foo foo" "foo" "bar"))
   (assert-equal "xxxx" (string-replace "xx" "x" "xx")))

  (test-mklist
   (assert-equal nil (mklist nil))
   (assert-equal '(a) (mklist 'a))
   (assert-equal '(a) (mklist '(a))))

  (test-iso-format-date
   (assert-equal "2012-10-11"
                 (iso-format-date '(20599 11146 317575))))

  (test-maybe-call
   (assert-equal '(a b c) (maybe-call 'list 'a 'b 'c))
   (assert-null (maybe-call 'list nil 'b 'c))
   (assert-null (maybe-call 'list 'a nil 'c))
   (assert-null (maybe-call 'list 'a 'b nil)))

  (test-sort-alist
   (assert-equal '(("a" . 1) ("b" . 2) ("c" . 3))
                 (sort-alist '(("c" . 3) ("a" . 1) ("b" . 2))
                             'string<))
   (assert-equal '(("a" . 1) ("b" . 2) ("b" . 2.5) ("c" . 3))
                 (sort-alist '(("c" . 3) ("b" . 2.5) ("a" . 1) ("b" . 2))
                             'string< '<)))

  (test-filename-split
   (assert-equal '()
                 (filename-split "/"))
   (assert-equal '("foo" "bar" "baz")
                 (filename-split "/foo/bar/baz"))
   (assert-equal '("foo" "bar" "baz" "")
                 (filename-split "/foo/bar/baz/")))

  (test-filename-join
   (assert-equal "/"
                 (filename-join '()))
   (assert-equal "/foo/bar/baz"
                 (filename-join '("foo" "bar" "baz")))
   (assert-equal "/foo/bar/baz/"
                 (filename-join '("foo" "bar" "baz" ""))))

  (test-take-while
   (assert-equal '(0 0) (take-while 'zerop '(0 0 1 2 3 0))))

  (test-take-until
   (assert-equal '(1 2) (take-until 'zerop '(1 2 0 3 4))))

  (test-drop-while
   (assert-equal '(1 2 3 0) (drop-while 'zerop '(0 0 1 2 3 0))))

  (test-drop-until
   (assert-equal '(0 3 4) (drop-until 'zerop '(1 2 0 3 4))))

  (test-buffer-lines
   (with-temp-buffer
     (insert "line1\nline2\nline3")
     (assert-equal '("line1" "line2" "line3")
                   (buffer-lines)))
   (with-temp-buffer
     (insert "line1\nline2\nline3\n")
     (assert-equal '("line1" "line2" "line3")
                   (buffer-lines)))
   (with-temp-buffer
     (insert "line1\nline2\nline3\n\n")
     (assert-equal '("line1" "line2" "line3" "")
                   (buffer-lines))))

  (test-buffer-lines-with-point
   (with-temp-buffer
     (insert "line1\nline2\nline3")
     (assert-equal '(("line1" . 1) ("line2" . 7) ("line3" . 13))
                   (buffer-lines-with-point)))
   (with-temp-buffer
     (insert "line1\nline2\nline3\n")
     (assert-equal '(("line1" . 1) ("line2" . 7) ("line3" . 13))
                   (buffer-lines-with-point)))
   (with-temp-buffer
     (insert "line1\nline2\nline3\n\n")
     (assert-equal '(("line1" . 1) ("line2" . 7) ("line3" . 13) ("" . 19))
                   (buffer-lines-with-point))))

  (test-map-to-list
   (assert-equal '(1 2 3)
                 (map-to-list 'mapc '(1 2 3))))

  (test-group-by
   (assert-equal '((0 . (2 4 6 8 10))
                   (1 . (1 3 5 7 9)))
                 (sort*
                  (group-by (lambda (x) (mod x 2))
                            '(1 2 3 4 5 6 7 8 9 10))
                  '<
                  :key 'car)))

  (test-if-string-match
   (assert-equal 1 (if-string-match ("[abc]" "a") 1 2 3))
   (assert-equal 3 (if-string-match ("[abc]" "A") 1 2 3))
   (assert-equal "a" (if-string-match ("<\\([abc]\\)>" "<a>")
                       (match-string 1)))
   (assert-equal "b" (if-string-match ("<\\([abc]\\)>" "<a>")
                       (if-string-match ("<\\([abc]\\)>" "<b>")
                         (match-string 1)))))

  (test-when-string-match
   (assert-equal 3 (when-string-match ("[abc]" "a") 1 2 3))
   (assert-nil (when-string-match ("[abc]" "z") 1 2 3)))

  (test-with-list-result
   (assert-equal '(a b c) (with-list-result r
                            (push 'a r)
                            (push 'b r)
                            (push 'c r))))

  (test-with-lexical-vars
   (let ((map1 (lambda (fn list)
                 (with-lexical-vars (fn)
                   (mapcar fn list))))
         (map2 (lambda (fn list)
                 (with-lexical-vars (fn)
                   (funcall map1
                            (lambda (x)
                              (funcall fn x))
                            list)))))
     (assert-equal '(2 4 6)
                   (funcall map2
                            (lambda (x) (* x 2))
                            '(1 2 3)))))

  (test-ancestor-dirs
   (assert-equal '("/usr/local/google/home/jrw/elisp/" "/usr/local/google/home/jrw/" "/usr/local/google/home/" "/usr/local/google/" "/usr/local/" "/usr/" "/")
                 (ancestor-dirs "/usr/local/google/home/jrw/elisp/")))

  (test-insert-with-markers
   (insert-with-markers "ab" 'point "cd" 'mark "ef")
   (assert-equal "abcdef" (buffer-string))
   (assert (looking-at "cd"))
   (goto-char (mark))
   (assert (looking-at "ef"))))

;; Local Variables:
;; no-byte-compile: t
;; End:
