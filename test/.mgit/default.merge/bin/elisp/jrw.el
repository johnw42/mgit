;; -*- no-byte-compile: t -*-
(require 'pcase)
(require 'rx)
(eval-when-compile (require 'cl))

(eval-and-compile
  (defun jrw/symbol-parts (symbol-or-string)
    (let ((name (if (stringp symbol-or-string)
                    symbol-or-string
                  (symbol-name symbol-or-string))))
      (cond
       ((or (string-match (rx string-start
                              (group (+ (not (any "/"))) "/")
                              (group (+ (not (any "/"))))
                              string-end)
                          name)
            (and (not (string-match (rx "---") name))
                 (not (string-match (rx "--" (* anything) "--") name))
                 (string-match (rx string-start
                                   (group (+ anything) "--")
                                   (group (+ anything))
                                   string-end)
                               name)))
        (list (match-string 1 name)
              (match-string 2 name)))
       (t
        (list "" name))))))

(eval-when-compile
  (assert (equal (jrw/symbol-parts "a/b") (list "a/" "b")) t)
  (assert (equal (jrw/symbol-parts "/b") (list "" "/b")) t)
  (assert (equal (jrw/symbol-parts "a/") (list "" "a/")) t)
  (assert (equal (jrw/symbol-parts "a/b/c") (list "" "a/b/c")) t)
  (assert (equal (jrw/symbol-parts "a//b") (list "" "a//b")) t)
  (assert (equal (jrw/symbol-parts "a--b") (list "a--" "b")) t)
  (assert (equal (jrw/symbol-parts "a-x--b") (list "a-x--" "b")) t)
  (assert (equal (jrw/symbol-parts "a--b-x") (list "a--" "b-x")) t)
  (assert (equal (jrw/symbol-parts "a-x--b-x") (list "a-x--" "b-x")) t)
  (assert (equal (jrw/symbol-parts "--b") (list "" "--b")) t)
  (assert (equal (jrw/symbol-parts "a--") (list "" "a--")) t)
  (assert (equal (jrw/symbol-parts "a--b--c") (list "" "a--b--c")) t)
  (assert (equal (jrw/symbol-parts "a---b") (list "" "a---b")) t))


(defmacro jrw/defstruct (symbol &rest slots)
  (declare (indent 1))
  (assert (symbolp symbol))
  (pcase-let* ((long-name (symbol-name symbol))
               (`(,prefix ,name) (jrw/symbol-parts long-name)))
    `(defstruct (,(intern (concat prefix name))
                 (:constructor ,(intern (concat prefix "make-" name)))
                 (:copier ,(intern (concat prefix "copy-" name))))
       ,@slots)))

;; (macroexpand '(jrw/defstruct jrw/struct slot1 slot2)
;;              '((defstruct))
;;              )

(provide 'jrw)

;; Local Variables:
;; no-byte-compile: t
;; End:
