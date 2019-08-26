(require 'my-hooks)
(require 'lisp-mode)

(defvar tmessage-file-name nil)

(defconst tmessage-grace-period 3
  "The number of days a tmessage should be displayed after it is
created.")

(defmacro tmessage (today-string format-string &rest args)
  "Display a message until a certain date.  The argument
TODAY-STRING should be todays date, in YYYY-MM-DD format."
  `(let ((tmessage-file-name load-file-name))
     (tmessage-impl ,today-string ,format-string ,@args)))

(defun tmessage-impl (today-string format-string &rest args)
  (let* ((deadline-string (format-time-string
                           "%Y-%m-%d"
                           (seconds-to-time
                            (+ (float-time)
                               (* tmessage-grace-period
                                  24 60 60))))))
    (when (string-lessp today-string deadline-string)
      (with-current-buffer "*Messages*"
        (let ((inhibit-read-only t))
          (insert
           (concat (propertize (apply 'format format-string args)
                               'face font-lock-function-name-face
                               'help-echo tmessage-file-name)
                   "\n")))))))

;; Define an abbreviation so that typing "tmessage" automatically
;; inserts the current date.
(define-abbrev emacs-lisp-mode-abbrev-table
  "tmessage"
  ""
  (defun tmessage-abbrev-hook ()
    (insert (format "tmessage \"%s\"" (format-time-string "%Y-%m-%d")))
    :system t))

(provide 'tmessage)
