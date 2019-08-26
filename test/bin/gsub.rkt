#! /bin/sh
#! -*- mode: racket -*-
#|
exec racket -l errortrace -u "$0" "$@"
|#

#lang racket

(require file/sha1)

(module+ test
  (require rackunit))

(define current-file-name (make-parameter (void)))
(define pattern-regexp (make-parameter (void)))
(define warning-count (box 0))

;; Command-line options.
(define files-to-process (make-parameter (void)))
(define pattern-string (make-parameter (void)
                                       (lambda (pattern)
                                         (pattern-regexp (pregexp pattern))
                                         pattern)))
(define replacement-string (make-parameter (void)))
(define backup-suffix (make-parameter #f))
(define patch-file-path (make-parameter (void)))
(define echo-mode (make-parameter 'files-changed))
(define keep-going-after-errors? (make-parameter #f))
(define update-files-in-place? (make-parameter #t))

(struct gnu-arg (flag-type flag flag-arg) #:transparent)

(define (parse-gnu-arg arg)
  ;; Parse a GNU-style argument into a |gnu-arg| structure.
  (cond
    [(equal? "--" arg)
     (gnu-arg 'separator #f #f)]
    [(regexp-match? #rx"^--[^=]+$" arg)
     (gnu-arg 'long arg #f)]
    [(regexp-match #rx"^(--[^=]+)=(.*)" arg) =>
     (match-lambda
       [(list _ flag flag-arg)
        (gnu-arg 'long flag flag-arg)])]
    [(regexp-match #rx"^(-.)(.+)?" arg) =>
     (match-lambda
       [(list _ flag flag-arg)
        (gnu-arg 'short flag flag-arg)])]
    [else (gnu-arg #f #f #f)]))

(module+ test
  (check-equal? (parse-gnu-arg "--foo=bar")
                (gnu-arg 'long "--foo" "bar"))
  (check-equal? (parse-gnu-arg "--foo=")
                (gnu-arg 'long "--foo" ""))
  (check-equal? (parse-gnu-arg "--foo")
                (gnu-arg 'long "--foo" #f))
  (check-equal? (parse-gnu-arg "-f")
                (gnu-arg 'short "-f" #f))
  (check-equal? (parse-gnu-arg "-f=oo")
                (gnu-arg 'short "-f" "=oo"))
  (check-equal? (parse-gnu-arg "-foo=")
                (gnu-arg 'short "-f" "oo="))
  (check-equal? (parse-gnu-arg "-foo")
                (gnu-arg 'short "-f" "oo"))
  (check-equal? (parse-gnu-arg "foo")
                (gnu-arg #f #f #f))
  (check-equal? (parse-gnu-arg "")
                (gnu-arg #f #f #f))
  (check-equal? (parse-gnu-arg "--")
                (gnu-arg 'separator #f #f)))

(define (args-from-gnu-style args)
  ;; Convert a sequence of GNU-style arguments into Racket-style
  ;; arguments.
  ;; TODO: Support clusters of short arguments.
  (let loop ([allow-flags? #t]
             [args (sequence->list args)]
             [out-flags null]
             [out-nonflags null])
    (if (null? args)
        (append (reverse out-flags) (list "--") (reverse out-nonflags))
        (let ([arg (first args)]
              [args (rest args)])
          (if (not allow-flags?)
              (loop #f args
                    out-flags
                    (cons arg out-nonflags))
              (match (parse-gnu-arg arg)
                [(gnu-arg #f _ _)
                 (loop #t args
                       out-flags
                       (cons arg out-nonflags))]
                [(gnu-arg 'separator _ _)
                 (loop #f args
                       out-flags
                       out-nonflags)]
                [(gnu-arg _ flag #f)
                 (loop #t args
                       (cons flag out-flags)
                       out-nonflags)]
                [(gnu-arg _ flag flag-value)
                 (loop #t args
                       (list* flag-value flag out-flags)
                       out-nonflags)]))))))

(module+ test
  (check-equal? (args-from-gnu-style #())
                '("--"))
  (check-equal? (args-from-gnu-style #("--"))
                '("--"))
  (check-equal? (args-from-gnu-style #("-a" "b" "-c"))
                '("-a" "-c" "--" "b"))
  (check-equal? (args-from-gnu-style #("-a" "--" "b" "-c"))
                '("-a" "--" "b" "-c"))
  (check-equal? (args-from-gnu-style #("-aaarg" "-bbarg"))
                '("-a" "aarg" "-b" "barg" "--"))
  (check-equal? (args-from-gnu-style #("--long1" "--long2=" "--long3=arg"))
                '("--long1" "--long2" "" "--long3" "arg" "--")))

;; Parses the command line, sets various parameters accordingly, and
;; returns a list of files to process.
(define (parse-command-line)
  (let ([reverse? #f]
        [fixed? #f])
    (command-line
     #:argv (args-from-gnu-style (current-command-line-arguments))
     
     #:once-each
     [("-i" "--backup-suffix") suffix
      "Create backup file by appending suffix (like perl -i)."
      (backup-suffix suffix)]
     [("-F" "--fixed-strings")
      "Treat <pattern> and <replacement> as literal strings (like grep -F)."
      (set! fixed? #t)]
     ["--reverse"
      "Invert the meaning of <pattern> and <replacement> (implies -F)."
      (set! reverse? #t)]
     [("-P" "--patch-file") file
      "Set the backup file to create."
      (patch-file-path (string->path file))]
     ["--no-patch-file"
      "Don't create a patch file."
      (patch-file-path #f)]

     #:once-any
     [("-u" "--undo") "Undo a previous command with the same arguments."
      (void)]
     [("-N" "--no-modify") "Don't modify files, just create a patch file."
      (update-files-in-place? #f)]
     [("-D" "--diff")
      ("Don't modify files or create a patch file, just show the diff.")
      (echo-mode 'diff)
      (update-files-in-place? #f)
      (patch-file-path #f)]

     #:args (pattern replacement . files)
     (let loop ([pattern pattern]
                [replacement replacement]
                [fixed? fixed?]
                [reverse? reverse?])
       (cond [reverse? (loop replacement pattern #t #f)]
             [fixed? (loop (regexp-quote pattern)
                           (regexp-replace-quote replacement)
                           #f #f)]
             [else (pattern-string pattern)
                   (replacement-string replacement)]))
     (files-to-process (map string->path files))
     (unless (patch-file-path)
       (patch-file-path (generate-patch-file-path)))))
  (files-to-process))

(module+ test
  (parameterize ([current-command-line-arguments #()])
    (check-exn
     exn:fail?
     parse-command-line)))

;; Debugging helper.  Prints and returns a value.
(define trace
  (case-lambda
    [(value)
     (trace #f identity value)]
    [(label-or-format-proc value)
     (if (string? label-or-format-proc)
         (trace label-or-format-proc identity value)
         (trace #f label-or-format-proc value))]
    [(label format-proc value)
     (eprintf "~a~v\n"
              (if (not label) "" (string-append label ": "))
              (format-proc value))
     value]))

(define (box-update! the-box update-proc)
  (let ([value (unbox the-box)])
    (box-cas! value (update-proc value))))

(define (generate-patch-file-path)
  (trace "filename" path->string
   (build-path
    (find-system-path 'temp-dir)
    (string-append
     "gsub."
     (sha1 (open-input-string (generate-patch-file-spec)))))))

(define (generate-patch-file-spec)
  (trace "path-spec"
   (format "~v ~v ~v"
           (sort
            (map (compose path->string path->complete-path)
                 (files-to-process))
            string<?)
           (pattern-string)
           (replacement-string))))

;; (define (init-pattern-and-replacement pattern replacement fixed-strings? reverse?)
;;   (cond
;;     [reverse?
;;      (init-pattern-and-replacement replacement pattern #t #f)]

;;     [fixed-strings?
;;      (init-pattern-and-replacement (regexp-quote pattern)
;;                                    (regexp-replace-quote replacement)
;;                                    #f #f)]

;;     [else
;;      (pattern-string pattern)
;;      (replacement-string replacement)]))


;; Returns #f if |path| is able to be update, or a warning string if
;; not.
(define (file-warning path)
  (let ([perms (delay (file-or-directory-permissions path))])
    (cond
      [(directory-exists? path)
       "is a directory"]
      [(not (file-exists? path))
       "no such file"]
      [(not (memq 'read (force perms)))
       "file is not readable"]
      [(not (memq 'write (force perms)))
       "file is not writable"]
      [else #f])))

;; Check all paths in |paths| for errors that will prevent them from
;; being updated.  Prints a warning for files that can't be updated,
;; and returns a list of files that are usable.
(define (check-files paths)
  (append*
   (for/list ([path paths])
     (match (file-warning path)
       [#f (list path)]
       [warning (warn-file path warning)]))))

;; Raises an error if execution should terminate because of warnings.
(define (check-errors)
  (when (and (positive? (unbox warning-count))
             (not (keep-going-after-errors?)))
    (raise-user-error "aborting because of warnings")))

;; (define (call-with-atomic-output-file filename proc)
;;   (define-values (dir-path last-part must-be-dir?) 
;;     (split-path (path->complete-path filename)))
;;   (define temp-file-path
;;     (make-temporary-file
;;      (string-append (path->string last-part) ".~a")))
;;   (dynamic-wind
;;     (lambda ())
;;     (lambda ()
;;       (with-handlers ([exn? (lambda (e)
;;                               (delete-file temp-file-path))])
;;         (call-with-output-file temp-file-path proc
;;           #:mode 'text #:exists 'must-truncate)))
;;     (lambda ()
;;       (when (file-exists? temp-file-path)
;;         (rename-file ))))
;;   )

(define (transform-file-content text)
  (string-replace text (pattern-regexp) (replacement-string)))

(define (run-diff old-path new-path)
  (define label (path->string old-path))
  (match-define-values
   (proc proc-stdout proc-stdin #f)
   (subprocess
    #f
    #f
    (current-error-port)
    (find-executable-path "diff")
    "-u"
    "--label" label
    "--label" label
    "--"
    (path->string old-path)
    (path->string new-path)))
  (close-output-port proc-stdin)
  (define diff-data (port->string proc-stdout))
  (displayln diff-data)
  (close-input-port proc-stdout))

(define (copy-with-backup from-path to-path)
  ;; Copies a file, creating backups according to the |backup-suffix|
  ;; parameter.
  (apply system*
         (find-executable-path "cp")
         (flatten
          (list
           "-f"
           (if (backup-suffix)
               (list "--suffix" (backup-suffix)
                     "--backup=numbered")
               null)
           "--"
           (path->string from-path)
           (path->string to-path)))))

(define (process-one-file path)
  (define input-data (file->string path))
  (define temp-file-path (make-temporary-file))
  (define diff-path (patch-file-path))
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (display-to-file (transform-file-content input-data)
                       temp-file-path #:exists 'truncate)
      (run-diff path temp-file-path)
      (when (update-files-in-place?)
        (unless
            (apply system*
                   (find-executable-path "cp")
                   (flatten
                    (list
                     "-f"
                     (if (backup-suffix)
                         (list "--suffix" (backup-suffix)
                               "--backup=numbered")
                         null)
                     "--"
                     (path->string temp-file-path)
                     (path->string path))))
          (error "copy failed"))))
    (lambda ()
      (delete-file temp-file-path))))

(define (process-files paths)
  (for ([path paths])
    (parameterize ([current-file-name path])
      (process-one-file path))))

;; Prints a warning about a file an increments the warning counter.
(define (warn-file path message)
  (eprintf "~a: ~a\n" path message)
  (box-update! warning-count add1))

(define (replace-in-files paths)
  (let ([paths (check-files paths)])
    (check-errors)
    (process-files paths))
  (void))

(module+ main
  (replace-in-files (parse-command-line)))
