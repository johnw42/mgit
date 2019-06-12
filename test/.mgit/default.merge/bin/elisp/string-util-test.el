;; -*- no-byte-compile: t -*-
(require 'string-util)
(require 'unit-test-core)

(unit-test-suite string-util-test
  (test-regexp-case
   (assert-equal 1 (regexp-case "a" ("a\\|b" 1)))
   (assert-equal 1 (regexp-case "b" ("a\\|b" 1)))
   (assert-equal 1 (regexp-case "ab" ("a" 1)))
   (assert-equal 1 (regexp-case "ab" ("b" 1)))
   (assert-nil (regexp-case "c" ("a\\|b" 1)))
   (assert-equal 3 (regexp-case "c" ("a" 1) ("b" 2) ("c" 3)))
   (assert-nil (regexp-case "x" ("a" 1) ("b" 2) ("c" 3)))
   (assert-equal 4 (regexp-case "x" ("a" 1) ("b" 2) ("c" 3) (t 4))))

  (test-string-join
   (assert-equal "a b c" (string-join '("a" "b" "c")))
   (assert-equal "a,b,c" (string-join '("a" "b" "c") ",")))

  (test-string-prefix-p
   (assert (string-prefix-p "" "abc"))
   (assert (string-prefix-p "a" "abc"))
   (assert (string-prefix-p "abc" "abc"))
   (assert-nil (string-prefix-p "c" "abc"))
   (assert-nil (string-prefix-p "abc" "a")))

  (test-string-suffix-p
   (assert (string-suffix-p "" "abc"))
   (assert (string-suffix-p "c" "abc"))
   (assert (string-suffix-p "abc" "abc"))
   (assert-nil (string-suffix-p "a" "abc"))
   (assert-nil (string-suffix-p "abc" "c")))

  (test-string-without-prefix
   (assert-equal "abc" (string-without-prefix "<<" "abc"))
   (assert-equal "abc" (string-without-prefix "<<" "<<abc")))

  (test-string-without-prefix-re
   (assert-equal "abc" (string-without-prefix-re "[xy]+" "xyabc"))
   (assert-equal "XYabc" (string-without-prefix-re "[xy]+" "XYabc")))

  (test-string-with-prefix
   (assert-equal "<<abc" (string-with-prefix "<<" "abc"))
   (assert-equal "<<abc" (string-with-prefix "<<" "<<abc")))

  (test-string-without-suffix
   (assert-equal "abc" (string-without-suffix ">>" "abc"))
   (assert-equal "abc" (string-without-suffix ">>" "abc>>")))

  (test-string-without-suffix-re
   (assert-equal "abc" (string-without-suffix-re "[xy]+" "abcxy"))
   (assert-equal "abcXY" (string-without-suffix-re "[xy]+" "abcXY")))

  (test-string-with-suffix
   (assert-equal "abc>>" (string-with-suffix ">>" "abc"))
   (assert-equal "abc>>" (string-with-suffix ">>" "abc>>")))

  (test-regexp-quote*
   (assert (string-match-p (regexp-quote* "+" "*") "+"))
   (assert (string-match-p (regexp-quote* "+" "*") "*"))))

;; Local Variables:
;; no-byte-compile: t
;; End:
