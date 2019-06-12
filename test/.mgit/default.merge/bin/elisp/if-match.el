;; -*- no-byte-compile: t -*-
(eval-when-compile (require 'cl))

(defvar if-match-matched-string nil)
(defvar if-match-match-data nil)

(defadvice match-string (before if-string-match activate)
  (when (and if-match-matched-string
             (null (ad-get-arg 1)))
    (setq if-match-match-data
          (match-data nil if-match-match-data))
    (when (integerp (car if-match-match-data))
      (ad-set-arg 1 if-match-matched-string))))

(defadvice match-string-no-properties (before if-string-match activate)
  (when (and if-match-matched-string
             (null (ad-get-arg 1)))
    (setq if-match-match-data
          (match-data nil if-match-match-data))
    (when (integerp (car if-match-match-data))
      (ad-set-arg 1 if-match-matched-string))))

(defmacro* if-match ((regex
                      &optional
                      (string nil string-given)
                      &key
                      (bound nil bound-given)
                      (count nil count-given)
                      start
                      literal)
                     if-match &rest if-not-match)
  "Search for a regular expression or literal string in a string or buffer.

Calls (`string-match' REGEX STRING) if STRING is specified,
or (`re-search-forward' REGEX) if not.  If there is a match, runs
IF-MATCH, otherwise runs IF-NOT-MATCH.  Within IF-MATCH, STRING
is implictly given as the second argument to `match-string' and
`match-string-no-properties'.  Matching is case-sensitive.

Match data is saved and matching is case-insensitive unless CASE
is specified and NIL.

UNTESTED FUNCTIONALITY:

If BOUND or COUNT are given, they are passed to
`re-search-forward' and STRING must not be given.

If START is given, it is passed to `string-match', and STRING
must be given.

UIMPLEMENTED FUNCTIONALITY:

If LITERAL is given, REGEX is passed through `regexp-quote'
before searching.
"
  (declare (indent 2)
           (debug ((body) body)))
  (assert (null literal))
  (if string-given
      (progn
        (assert (not bound-given))
        (assert (not count-given))
        `(let ((if-match-matched-string ,string))
           (save-match-data
             (if (let ((case-fold-search nil))
                   (string-match ,regex if-match-matched-string ,start))
                 ,if-match
               ,@if-not-match))))
    `(progn
       (when ,start
         (goto-char (or ,start (point))))
       (if (let ((case-fold-search nil))
             (re-search-forward ,regex ,bound t ,count))
           ,if-match
         ,@if-not-match))))

(defmacro* when-match ((regex &rest rest) &rest body)
  (declare (indent 1)
           (debug ((body) body)))
  `(if-match (,regex ,@rest) (progn ,@body)))

(defmacro* while-match ((regex &rest rest) &rest body)
  (declare (indent 1)
           (debug ((body) body)))
  `(while (if-match (,regex ,@rest) (progn ,@body t))))

(provide 'if-match)

;; Local Variables:
;; no-byte-compile: t
;; End:
