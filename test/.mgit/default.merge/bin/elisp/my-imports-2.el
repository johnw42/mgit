;; -*- no-byte-compile: t -*-
(require 'jrw)

;;;
;;; Mandatory buffer-local settings.
;;;

(make-variable-buffer-local
 (defvar my-imports-parse-import-line-function nil
   "Mandatory function to parse a line of text into a
`my-imports-parsed' structure.  The function is called
with one argument, the line to parse.  It should return
nil for non-import lines.  The point will be set to
the beginning of the line being parsed."))

(make-variable-buffer-local
 (defvar my-imports-make-import-line-function nil
   "Mandatory function to generate an import line from a
`my-imports-parsed' structure."))

;;;
;;; Optional buffer-local settings.
;;;

(make-variable-buffer-local
 (defvar my-imports-default-input-function nil
   "Optional function to compute the default input for an
interactive import command.  Must return a suitable value for the
INITIAL-INPUT parameters of `completing-read'."))

(make-variable-buffer-local
 (defvar my-imports-import-history-var nil
   "Optional mode-specific history variable to use for imports.
Defaults to a variable based on the major mode."))

(make-variable-buffer-local
 (defvar my-imports-import-completions-function nil
   "Optional completion function to use with
`completing-read'."))

(make-variable-buffer-local
 (defvar my-imports-expand-name-function nil
   "Optional function to expand a simple name into a
fully-qualified name.  Only used if
`my-imports-default-input-function' is nil."))

(make-variable-buffer-local
 (defvar my-imports-buffer-package-function nil
   "Optional function to get the package name of the current
buffer.  By default, the current buffer has no package."))

(make-variable-buffer-local
 (defvar my-imports-parse-import-namespec-function nil
   "Optional function to parse a name specification and turn it
into a `my-imports-parsed' structure.  By default, the entire
namespec is treated as a package name."))

;; (make-variable-buffer-local
;;  (defvar my-imports-make-import-namespec-function nil
;;    "Optional function to create a `my-imports-parsed' structure
;; from a namespec string.  By default, the namespec is just the package name."))

(make-variable-buffer-local
 (defvar my-imports-go-to-default-import-position-function nil
   "Optional.  By default, imports are placed at the start of the
file when there are no existing imports."))

(make-variable-buffer-local
 (defvar my-imports-sort-and-group-imports-function nil
   "Optional.  This function should take a list of
`my-imports-parsed' structures and return a sorted list of
lists."))

(make-variable-buffer-local
 (defvar my-imports-sort-imports-function nil
   "Optional.  By default, import statements are sorted
naively."))

(make-variable-buffer-local
 (defvar my-imports-import-group-function nil
   "Optional function to map a `my-imports-parsed' structure to a
group name.  By default, all imports belong to a single group."))

(make-variable-buffer-local
 (defvar my-imports-name-at-point-function nil
   "Optional function to guess a name to import based on the
location of the point.  Defaults to symbol at point.  Only used
if `my-imports-default-input-function' is nil."))

(make-variable-buffer-local
 (defvar my-imports-initial-input-function nil
   "Not used."))

(defvar my-imports-after-add-import-hook nil)

;;;
;;; Implementation.
;;;

(defstruct my-imports-parsed package class member local-name)

(defvar jrw/import-groups (make-hash-table :test 'equal :weakness 'key))

(defun my-imports-interactive-import (namespec)
  "Add an import/require statement to a file."
  (interactive
   (list (completing-read
          ;; prompt
          "Name to import: "
          ;; collection
          (and my-imports-import-completions-function
               (funcall my-imports-import-completions-function))
          nil                           ; predicate
          nil                           ; require-match
          ;; initial-input
          (my-imports-default-input)
          ;; hist
          (my-imports-import-history-var)
          ;; def
          nil)))
  (my-imports-interactive-import-1 namespec))

(defun my-imports-interactive-import-1 (namespec)
  "Add an import statement to a Java file."
  (let* ((parsed-import (my-imports-parse-import-namespec namespec))
         (case-fold-search nil)
         why
         message
         package-import
         import-added)

    (setq why (my-imports-add-import parsed-import))
    (if (not why)
        (setq import-added t)
      (setq message (case why
                      (conflict
                       (format "%s conflicts with a name already in use"
                               namespec))
                      (t (format "%s is already %s"
                                 namespec
                                 (case why
                                   (wildcard "imported using a wildcard")
                                   (package "defined in this package")
                                   (name "imported by name"))))))
      (if (eq why 'wildcard)
          (when (y-or-n-p (format "%s; import anyway? " message))
            (my-imports-add-import parsed-import t)
            (setq import-added t))
        (message "%s." message)))
    (when import-added
      (message "Import statement added."))))

(defun my-imports-how-imported (arg-import)
  (assert (my-imports-parsed-p arg-import))
  (let* ((buffer-package (my-imports-buffer-package))
         (arg-member (my-imports-parsed-member arg-import))
         (arg-class (my-imports-parsed-class arg-import))
         (arg-package (my-imports-parsed-package arg-import)))
    (if (and (not arg-member)
             (stringp buffer-package)
             (equal buffer-package arg-package))
        'package
      (catch 'how
        (dolist (found-import (my-imports-parse-buffer-imports))
          (assert (my-imports-parsed-p found-import))
          (let* ((found-member (my-imports-parsed-member found-import))
                 (found-class (my-imports-parsed-class found-import))
                 (found-package (my-imports-parsed-package found-import))
                 (same-package (equal found-package arg-package))
                 (same-class (and same-package
                                  (equal found-class arg-class)))
                 (same-member (equal found-member arg-member)))
            (cond
             ((and (not found-class) (not arg-class))
              (when same-package
                (throw 'how 'name)))
             ((and (not found-member) (not arg-member))
              (when (eq 'wildcard found-class)
                (when same-package
                  (throw 'how 'wildcard)))
              (when same-class
                (throw 'how (if same-package 'name 'conflict))))
             ((and found-member arg-member)
              (when (eq 'wildcard found-member)
                (when same-class
                  (throw 'how 'wildcard)))
              (when same-member
                (throw 'how (if same-class 'name 'conflict)))))))))))

(defun my-imports-add-import (new-import &optional force)
  "Add a new import statement if possible.  The value of
NEW-IMPORT can be a parsed import or a string."
  (when (stringp new-import)
    (setq new-import (my-imports-parse-import-namespec new-import)))
  (let* ((package-name (my-imports-parsed-package new-import))
         (class-name (my-imports-parsed-class new-import))
         (member-name (my-imports-parsed-member new-import))
         (imports (my-imports-parse-buffer-imports))
         import-found)

    (or (and (not force)
             (my-imports-how-imported new-import))
        (if imports
            (let* ((new-import-groups (my-imports-sort-and-group-imports
                                       (cons new-import imports)))
                   (new-imports (apply 'append new-import-groups))
                   (_ (assert (= (length new-imports)
                                 (1+ (length imports)))))
                   (pos (position new-import new-imports :test 'equal))
                   (_ (assert pos))
                   (next-pos (1+ pos))
                   (next-import (and (< next-pos (length new-imports))
                                     (elt new-imports next-pos))))
              (if (and next-import
                       (or (zerop pos)
                           (my-imports-same-import-group-p new-import
                                                           next-import)))
                  (my-imports-insert-import-before next-import
                                                   new-import)
                (my-imports-insert-import-after (elt new-imports (1- pos))
                                                new-import)))
          (save-excursion
            (save-restriction
              (widen)
              (if my-imports-go-to-default-import-position-function
                  (funcall my-imports-go-to-default-import-position-function)
                (goto-char (point-min)))
              (my-imports-insert-import new-import))))))
  (run-hooks 'my-imports-after-add-import-hook))

(defun my-imports-sort-imports (parsed-imports)
  (assert (every 'my-imports-parsed-p parsed-imports))
  (if my-imports-sort-imports-function
      (funcall my-imports-sort-imports-function
               parsed-imports)
    (sort* (list* parsed-imports) 'string-lessp
           :key 'my-imports-make-import-line)))

(defun my-imports-import-group (parsed-import)
  (assert (my-imports-parsed-p parsed-import))
  (or (gethash parsed-import jrw/import-groups)
      (puthash parsed-import
               (if my-imports-import-group-function
                   (funcall my-imports-import-group-function parsed-import)
                 "")
               jrw/import-groups)))

(defun my-imports-group-imports (parsed-imports)
  "Given a list of parsed imports, return a list of lists."
  (assert (every 'my-imports-parsed-p parsed-imports))
  (let* ((grouped-imports
          (stable-sort
           (group-by 'my-imports-import-group
                     parsed-imports
                     :test 'equal)
           'string-lessp :key 'car)))
    (mapcar 'cdr grouped-imports)))

(defun my-imports-same-import-group-p (parsed-import-1 parsed-import-2)
  "Returns t of both parsed imports belong to the same group."
  (equal (my-imports-import-group parsed-import-1)
         (my-imports-import-group parsed-import-2)))

(defun my-imports-sort-and-group-imports (parsed-imports)
  (if my-imports-sort-and-group-imports-function
      (let ((groups (funcall my-imports-sort-and-group-imports-function parsed-imports))
            (group-index 0))
        (dolist (group groups)
          (incf group-index)
          (dolist (import group)
            (puthash import group-index jrw/import-groups)))
        groups)
    (my-imports-group-imports
     (my-imports-sort-imports
      parsed-imports))))

(defun my-imports-parse-buffer-imports (&optional buffer)
  "Parses and returns a list of all the import statements in
BUFFER."
  (with-list-result imports
    (dolist (pair (buffer-lines-with-point buffer))
      (save-excursion
        (goto-char (cdr pair))
        (let ((import (my-imports-parse-import-line)))
          (when import
            (push import imports)))))))

(defun my-imports-find-parsed-import (parsed-import)
  "Finds and returns the character offset of the first import
  line that parses to PARSED-IMPORT."
  (assert (my-imports-parsed-p parsed-import))
  (catch 'found
    (map-buffer-lines
     (lambda (line)
       (let ((parsed-line (my-imports-parse-import-line)))
              (when (equal parsed-import parsed-line)
                (throw 'found (point))))))))

(defun my-imports-parse-import-line (&optional line)
  "Parse the current line as an import.  Returns nil if the current line is not an import."
  (save-excursion
    (beginning-of-line)
    (funcall my-imports-parse-import-line-function
             (or line
                 (buffer-substring (point)
                                   (save-excursion (end-of-line)
                                                   (point)))))))

(defun my-imports-insert-import (parsed-import)
  (assert (my-imports-parsed-p parsed-import))
  (let ((prev-line (save-excursion
                     (if (zerop (forward-line -1))
                         (thing-at-point 'line)
                       "\n"))))
    (unless (equal "\n" prev-line)
      (let ((prev-import (my-imports-parse-import-line prev-line)))
        (unless (and prev-import
                     (my-imports-same-import-group-p parsed-import prev-import))
          (insert "\n")))))
  (insert (my-imports-make-import-line parsed-import) "\n")
  (let ((next-line (thing-at-point 'line)))
    (unless (equal "\n" next-line)
      (let ((next-import (my-imports-parse-import-line next-line)))
        (unless (and next-import
                     (my-imports-same-import-group-p parsed-import next-import))
          (insert "\n"))))))

(defun my-imports-make-import-line (parsed-import)
  (funcall my-imports-make-import-line-function parsed-import))

(defun my-imports-insert-import-after (prev-import new-import)
  (assert (my-imports-parsed-p prev-import))
  (assert (my-imports-parsed-p new-import))
  (let ((loc (my-imports-find-parsed-import prev-import)))
    (assert loc)
    (save-excursion
      (goto-char loc)
      (forward-line 1)
      (my-imports-insert-import new-import))))

(defun my-imports-insert-import-before (next-import new-import)
  (assert (my-imports-parsed-p next-import))
  (assert (my-imports-parsed-p new-import))
  (let ((loc (my-imports-find-parsed-import next-import)))
    (assert loc)
    (save-excursion
      (goto-char loc)
      (my-imports-insert-import new-import))))

;; XXX delete?
;; (defun my-imports-parse-buffer-imports ()
;;   (with-list-result r
;;     (dolist (line (buffer-lines))
;;       (let ((parsed (my-imports-parse-import-line line)))
;;         (when parsed
;;           (push parsed r))))))

(defun my-imports-buffer-package ()
  (and my-imports-buffer-package-function
       (funcall my-imports-buffer-package-function)))

(defun my-imports-import-history-var ()
  (or my-imports-import-history-var
      (let ((var (intern (format "my-imports-%S-history" major-mode))))
        (eval `(defvar ,var nil)))))

(defun my-imports-parse-import-namespec (namespec)
  (if my-imports-parse-import-namespec-function
      (funcall
       my-imports-parse-import-namespec-function
       namespec)
    (make-my-imports-parsed :package namespec)))

(defun my-imports-expand-name (name)
  (and name
       my-imports-expand-name-function
       (funcall my-imports-expand-name-function name)))

(defun my-imports-name-at-point ()
  (if my-imports-name-at-point-function
      (funcall my-imports-name-at-point-function)
    (thing-at-point 'symbol)))

(defun my-imports-default-input ()
  (if my-imports-default-input-function
      (funcall my-imports-default-input-function)
    (let* ((symbol (my-imports-name-at-point))
           (expanded-name (my-imports-expand-name symbol))
           (package (my-imports-buffer-package)))
      (cond
       (expanded-name
        (cons expanded-name
              (- (length expanded-name) (length symbol))))
       ((and symbol package)
        (cons (concat package "." symbol)
              (1+ (length package))))
       (symbol
        (cons symbol 0))))))

(provide 'my-imports)

;; Local Variables:
;; no-byte-compile: t
;; End:
