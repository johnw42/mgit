;; -*- no-byte-compile: t -*-
;; -*- lexical-binding: t; -*-
(require 'elide)
(require 'jasmel)
(elide/ns elide
  (:import example2 example2-function))

(message "Loading elide-mode")

(with-no-warnings
  (require 'cl))
(require 'thingatpt)

(defmacro elide/no-compile (&rest body)
  `(eval '(progn ,@body)) t)

(callf2 assq-delete-all 'elide-mode format-alist)
(push '(elide-mode
        "Emacs Lisp IDE"
        nil
        elide-mode-decode
        elide-mode-encode
        t nil nil nil)
      format-alist)

;; (defvar elide/file-local-variables
;;   '((elide/namespace-prefix . stringp)))

;; (dolist (pair elide/file-local-variables)
;;   (put (car pair)
;;        'safe-local-variable
;;        (cdr pair)))



(defconst elide/elide-mode-debug nil)
(defconst elide/expand-macros t)
(defvar elide/show-preview t)
(defconst elide/namespace-separator "/")

(defvar elide/content-beginning)
(defvar elide/content-end)
(defvar elide/imports nil)
(defvar elide/resolved-symbols nil)

(defvar elide/exported-names nil
  "A buffer-local hash table mapping each exported name string to
t.")

(defvar elide/env)
;; (defvar elide/module-name)
;; (defvar elide/expansions nil)
(defconst elide/annotation-face 'shadow)
(defconst elide/protected-face 'shadow)
(defconst elide/local-name-face 'underline)

(defconst elide/font-lock-keywords
  (append
   lisp-font-lock-keywords-2
   '((elide/search-local-name . (0 elide/local-name-face prepend)))
   ;; '(elide/search-annotation . (0 elide/annotation-face prepend))
   ;; `(,elide/protect-pattern . (0 elide/protected-face prepend))
   ))

(defstruct elide/import
  name emacs-prefix local-prefix imported-names)

(defmacro elide/debug-vars (&rest names)
  `(progn
     ,@(mapcar (lambda (name)
                 `(defvar ,name))
               names)))

(defun elide/search-local-name (limit)
  (let* ((search-start (point))
         (props-here (text-properties-at search-start))
         (prop-start (if (plist-get props-here 'elide/resolved)
                         search-start
                       (next-single-property-change
                        search-start 'elide/resolved nil limit)))
         (prop-end (and prop-start
                        (or (next-single-property-change
                             prop-start 'elide/resolved nil limit)
                            limit))))
    (when prop-end
      ;;(message "elide/search-local-name %s-%s %s-%s" search-start limit prop-start prop-end)
      (set-match-data (list prop-start prop-end))
      (goto-char prop-end)
      t)))

(defconst elide/elide-mode-hooks '((change-major-mode-hook elide/change-major-mode-hook)))

(defconst elide/invisibility-spec '(elide/local-symbol))

(define-derived-mode elide-mode emacs-lisp-mode "Elide"
  "Emacs Lisp IDE major mode."
  (setq font-lock-defaults
        (list* '(elide/font-lock-keywords)
               (cdr font-lock-defaults)))
  (dolist (hook elide/elide-mode-hooks)
    (add-hook (car hook) (cadr hook) nil 'local))
  (unless (memq 'elide-mode buffer-file-format)
    (format-decode-buffer 'elide-mode))
  (elide/analyze-buffer)
  )

(define-key elide-mode-map (kbd "C-c x") 'elide/export-symbol)
(define-key elide-mode-map (kbd "C-c C-x") 'elide/export-symbol)

(defun elide/change-major-mode-hook ()
  (elide/save-modified 
   ;; Probably not necessary because of kill-all-local-variables.
   (dolist (hook elide/elide-mode-hooks)
     (remove-hook (car hook) (cadr hook) 'local))
   (when (memq 'elide-mode buffer-file-format)
     (format-encode-buffer 'elide-mode)
     (callf2 remq 'elide-mode buffer-file-format))))

(defun elide/resolve-symbol (elide-name)
  "Converts an elide surface form of a symbol to the emacs form."
  (or
   (gethash elide-name elide/resolved-symbols)
   (puthash 
    elide-name
    (let* ((pair (elide/decompose-symbol elide-name))
           (qualifier (car pair))
           (name (cdr pair)))
      (if qualifier
          (or
           ;; Handle names qualified with emacs/
           (and (equal qualifier "emacs") (intern name))
           ;; Handle names qualified with an imported namespace.
           (catch 'found
             (dolist (import (cdr elide/imports))
               (let ((sym (elide/intern-soft
                           (elide/import-name import)
                           elide-name)))
                 (when sym
                   (throw 'found sym)))))
           (intern elide-name))
        (or
         ;; Handle unqualified  names defined in this file.
         (and (gethash elide-name elide/exported-names)
              (intern (concat elide/namespace-prefix elide-name)))
         ;; Handle unqualified names imported from elsewhere.
         (catch 'matched
           (dolist (import (cdr elide/imports))
             (let ((stripped
                    (elide/strip-prefix
                     elide-name
                     (elide/import-local-prefix import))))
               (when stripped
                 (throw 'matched
                        (intern (concat (elide/import-emacs-prefix import)
                                        stripped)))))))
         ;; Use the name as-is.
         (intern elide-name))))
    elide/resolved-symbols)))

(defun elide/find-defined-names ()
  (elide/map-forms
   (lambda (form _form-beginning _form-end _form-symbols)
     (elide/mapform
      form :pre-proc
      (lambda (form)
        (when (and (consp form)
                   (symbolp (cadr form))
                   (not (gethash (symbol-name (cadr form)) elide/exported-names))
                   (memq (car form) '(defun defmacro defsubst
                                       defun* defmacro* defsubst*
                                       defvar defconst)))
          (puthash (symbol-name (cadr form)) t
                   elide/exported-names)))))))

(defun elide/analyze-buffer ()
  ;; (set (make-local-variable 'elide/exported-names)
  ;;      (make-hash-table :test 'equal))
  (set (make-local-variable 'elide/resolved-symbols)
       (make-hash-table :test 'equal))
  (elide/save-modified
   (save-excursion
     (save-restriction
       (widen)
       (remove-text-properties (point-min)
                               (point-max)
                               (list 'help-echo nil
                                     'point-enetered nil
                                     'elide/resolved nil))
       (goto-char (point-min))
       (elide/detect-namespaces)
       (elide/find-defined-names)
       (elide/update-namespaces)
       (let ((inhibit-read-only t))
         (elide/propertize-symbols
          (lambda (elide-name)
            (let* ((resolved (elide/resolve-symbol elide-name))
                   (resolved-name (symbol-name resolved))
                   new-props)
              (unless (equal elide-name resolved-name)
                (callf append new-props
                  (list 'help-echo (symbol-name (elide/resolve-symbol elide-name))
                        'point-entered 'elide/help-echo-hook
                        'elide/resolved resolved)))
              new-props))))))))

(defvar elide/help-echo-message nil)

(defun elide/help-echo-hook (_old-point new-point)
  (let ((help-message (get-text-property new-point 'help-echo)))
    (when (and help-message
               (not (equal help-message elide/help-echo-message)))
      (message "%s" help-message))))

(defun elide/resolve-symbols ()
  (elide/transform-symbols
   (lambda (symbol-text)
     (let* ((elide-name symbol-text))
       (symbol-name (elide/resolve-symbol elide-name))))))

(defun elide-mode-encode (from to orig-buf)
  "Convert Elide code into plain Elisp code."
  (dolist (local-var '(elide/exported-names))
    (set (make-local-variable local-var)
         (buffer-local-value local-var orig-buf)))
  (elide/analyze-buffer)
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    ;; (add-file-local-variable-prop-line 'lexical-binding t)
    ;; (delete-file-local-variable-prop-line 'mode)
    ;; (add-file-local-variable-prop-line 'mode 'elide)
    ;; (add-file-local-variable 'elide/namespace-prefix
    ;;                          elide/namespace-prefix)
    (elide/resolve-symbols)

    ;; (elide/transform-symbols
    ;;  (lambda (symbol-text)
    ;;    (let* ((elide-name (substring-no-properties symbol-text))
    ;;           (props (text-properties-at 0 symbol-text))
    ;;           (qualified (plist-get props 'elide/qualified)))
    ;;      (when qualified
    ;;        (if (string-match "emacs/\\(.*\\)" elide-name)
    ;;            (match-string 1 elide-name)
    ;;          (concat (symbol-name elide/namespace) "-" elide-name))))))
    
    ;; (while (not (eobp))
    ;;   (let* ((region-start (point))
    ;;          (region-end (or (next-single-property-change
    ;;                           region-start 'elide/qualified)
    ;;                          (point-max)))
    ;;          (all-props (text-properties-at region-start))
    ;;          (exported (plist-get all-props 'elide/qualified)))
    ;;     (goto-char region-end)
    ;;     (when exported
    ;;       (save-excursion
    ;;         (goto-char region-start)
    ;;         (if (looking-at "emacs/")
    ;;             (delete-char 6)
    ;;           (insert (symbol-name elide/namespace) "-"))))))
    ;; (remove-text-properties (point-min) (point-max)
    ;;                         '(elide/qualified nil font-lock-face nil))
    (point-max)))

(defun elide-mode-decode (from to)
  "Convert plain Elisp code into Elide code."
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      ;; (forward-line 2)
      ;; (delete-region (point-min) (point))
      (elide/detect-namespaces)
      (elide/abbreviate-symbols)
      (point-max))))

(defun elide/eval-on-decode-form-p (form)
  (eq 'elide/ns (car-safe form)))

(defun elide/process-namespace (namespace &rest forms)
  (assert (null elide/namespace-name))
  (assert (symbolp namespace))
  (setq elide/namespace-name
        (symbol-name namespace))
  (setq elide/namespace-prefix
        (concat elide/namespace-name
                elide/namespace-separator))
  (dolist (form forms)
    (case (car-safe form)
      ((:prefix)
       (assert (stringp (cadr form)))
       (assert (null (cddr form)))
       (setq elide/namespace-prefix
             (cadr form)))
      (:import
       (apply 'elide/process-namespace-import (cdr form)))
      (:export
       (apply 'elide/process-namespace-export (cdr form)))))
  
  (callf nreverse elide/imports)
  (push
   (elide/make-import :name elide/namespace-name
                      :emacs-prefix elide/namespace-prefix
                      :local-prefix "")
   elide/imports)
  nil)

(defun elide/process-namespace-import (_ns &rest _import-names)
  
  )

(defun elide/process-namespace-export (&rest names)
  
  )

(defun elide/detect-namespaces ()
  (set (make-local-variable 'elide/namespace-name) nil)
  (set (make-local-variable 'elide/namespace-prefix) nil)
  (set (make-local-variable 'elide/imports) nil)
  (set (make-local-variable 'elide/exported-names) (make-hash-table :test 'equal))
  (elide/map-forms
   (lambda (form _form-beginning _form-end _form-symbols)
     (when (elide/eval-on-decode-form-p form)
       (apply 'elide/process-namespace (cdr form))))))

(defun elide/has-prefix-p (string prefix)
  (and (< (length prefix)
          (length string))
       (string= (substring string 0 (length prefix))
                prefix)))

(defun elide/strip-prefix (string prefix)
  (and (elide/has-prefix-p string prefix)
       (substring string (length prefix))))

(defun elide/index-by-plist-value (old-table plist-key)
  (let ((new-table (make-hash-table :test 'equal)))
    (maphash (lambda (_old-key value)
               (let ((new-key (plist-get value plist-key)))
                 (when new-key
                   (puthash new-key value new-table))))
             old-table)
    new-table))

(defun elide/update-hash (function table)
  "Calls FUNCTION on each (key, value) pair of TABLE, and update
the value stored in TABLE with the returned value.  Returns
TABLE."
  (let (updates)
    (maphash (lambda (key value)
               (let ((new-value (funcall function key value)))
                 (unless (eq value new-value)
                   (push (cons key new-value) updates))))
             table)
    (dolist (update updates)
      (puthash (car update) (cdr update) table)))
  table)

(defun elide/hash-values (table)
  (let (result)
    (maphash (lambda (key value)
               (push (cons key value) result))
             table)
    (sort result (lambda (elide/x y)
                   (string< (car elide/x) (car y))))))

(defun elide/abbreviate-symbols ()
  ;; (set (make-local-variable 'elide/exported-names)
  ;;      (make-hash-table :test 'equal))
  (let ((emacs-names (make-hash-table :test 'equal))
        elide/names
        ;; List of namespaces sorted in descending order of
        ;; prefix length.
        (namespaces (sort 
                     elide/imports
                     (lambda (lhs rhs)
                       (> (length (elide/import-emacs-prefix lhs))
                          (length (elide/import-emacs-prefix rhs)))))))
    (elide/map-forms
     (lambda (form _form-beginning _form-end form-symbols)
       (unless (elide/eval-on-decode-form-p form)
         (dolist (symbol-info form-symbols)
           (let ((emacs-name (car symbol-info)))
             (puthash emacs-name
                      (list :emacs-name emacs-name)
                      emacs-names))))))
    ;; Assign an elide-name to every symbol with a prefix.
    (elide/update-hash (lambda (emacs-name value)
                         (dolist (ns namespaces)
                           (let* ((prefix (elide/import-emacs-prefix ns))
                                  (elide-name (elide/strip-prefix emacs-name prefix)))
                             (when elide-name
                               (callf plist-put value :elide-name elide-name) 
                               (return t))))
                         value)
                       emacs-names)
    (setq elide/names (elide/index-by-plist-value emacs-names :elide-name))
    (maphash
     (lambda (key _value)
       (puthash key t elide/exported-names))
     elide/names)
    ;; Assign an elide-name to symbols with no prefix.
    (elide/update-hash (lambda (emacs-name value)
                         (if (plist-get value :elide-name)
                             value
                           (callf plist-put value :elide-name
                                  (if (gethash emacs-name elide/names)
                                      (concat "emacs/" emacs-name)
                                    emacs-name))))
                       emacs-names)
    ;; Transform emacs-names into elide/names.
    (elide/transform-symbols
     (lambda (old-text)
       (let* ((emacs-name old-text)
              (plist (gethash emacs-name emacs-names))
              (elide-name (plist-get plist :elide-name)))
         (unless elide-name
           (error "No elide name for emacs-name: %s" emacs-name))
         (unless (string= emacs-name elide-name)
           (propertize elide-name
                       'elide/qualified t)))))))

(defun elide/map-symbols (fn)
  (elide/map-forms
   (lambda (form _form-beginning _form-end form-symbols)
     (unless (elide/eval-on-decode-form-p form)
       (dolist (symbol-info form-symbols)
         (apply fn symbol-info))))))

(defun elide/transform-symbols (fn)
  "Transforms all the symbols in the file.  For each occurrence
of a symbol, FN is called with the symbol's textual
representation, complete with text properties.  If FN returns a
string, the occurrence of the symbol is replaced with that
string."
  (elide/map-symbols
   (lambda (_sym start end)
     (let* ((old-name (buffer-substring start end))
            (new-name (funcall fn old-name)))
       (when (and new-name
                  (not (equal old-name new-name)))
         (with-no-warnings
           (setf (buffer-substring start end) new-name)))))))

(defun elide/propertize-symbols (fn)
  (elide/save-modified
   (let ((inhibit-read-only t))
     (elide/map-symbols
      (lambda (symbol start end)
        (let ((props (funcall fn symbol)))
          (when props
            (add-text-properties start end props))))))))

(defmacro elide/save-modified (&rest body)
  `(let ((was-modified (buffer-modified-p))
         (inhibit-read-only t))
     (unwind-protect
         (save-excursion ,@body)
       (set-buffer-modified-p was-modified))))

(defun elide/mark-annotation (beginning end)
  (add-text-properties beginning end
                       (list 'elide/annotation t
                             'read-only t
                             'front-sticky t
                             )))

;; (defun elide/map-sexps (f)
;;   "For each top-level form in the module, call F.  The arguments
;; to F are BEG, a marker at the beginng on the form, END, a marker
;; at the end of the form, and FORM, the parsed form itself."
;;   (let (result beg end)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (re-search-forward "^(elide/module ")
;;       (forward-sexp)
;;       (condition-case nil
;;           (while t
;;             (forward-sexp)
;;             (setq end (point-marker))
;;             (backward-sexp)
;;             (setq beg (point-marker))
;;             (goto-char end)
;;             (funcall f beg end
;;                      (read (buffer-substring beg end))))
;;         (scan-error nil)))))

;; (defun elide/annotate-defuns ()
;;   (when (listp elide/expansions)
;;     (save-excursion
;;       (elide/map-sexps
;;        (lambda (sexp-begin sexp-end sexp)
;;          (goto-char sexp-end)
;;          (forward-line)
;;          (elide/insert-annotation
;;           ;;(cl-prettyprint (elide/strip-strings (pop expansions)))
;;           (format "%s" (with-temp-buffer (cl-prettyprint (elide/strip-strings
;;                                                           (pop elide/expansions)))
;;                                          (buffer-string)))
;;           ;;(format "annotation for %s" (if (listp sexp) (car sexp) sexp))
;;           ))))))

;; (defun elide/annotate-buffer ()
;;   ;;(elide/add-boilerplate)
;;   ;;(elide/find-annotations)
;;   )

;; (defun elide/delete-line ()
;;   (save-excursion
;;     (beginning-of-line)
;;     (let ((beg (point)))
;;       (forward-line)
;;       (delete-region beg (point)))))

;; (defun elide/delete-protected-lines (tag)
;;   (let ((pattern (elide/protect-marker-pattern tag)))
;;     (if (not (re-search-forward pattern nil t))
;;         nil
;;       (save-excursion
;;         (elide/delete-line)
;;         (while (re-search-forward pattern nil t)
;;           (elide/delete-line)))
;;       t)))

;; (defun elide/insert-protected (text tag)
;;   (assert (bolp))
;;   (let ((marker (elide/protect-marker tag))
;;         (old-point (point)))
;;     (insert (with-temp-buffer
;;               (insert text "\n")
;;               (goto-char (point-min))
;;               (while (not (eobp))
;;                 (end-of-line)
;;                 (insert marker)
;;                 (forward-line))
;;               (buffer-string)))
;;     (elide/mark-annotation old-point (1- (point)))))

(defun elide/make-default-env ()
  (let ((env (make-hash-table :test 'eq)))
    (dolist (def '((import . (elide/expand-import))
                   (defstruct . (elide/expand-defstruct))))
      (puthash (car def) (cdr def) env))
    env))

;; (defun elide/qualify-name (name)
;;   (let ((qname (make-symbol (format "%s-%s" elide/module-name name))))
;;     (puthash name qname elide/env)
;;     qname))

;; (defun elide/expand-defstruct (name &rest fields)
;;   `((defstruct (,(elide/qualify-name name)
;;                 (:constructor ,(elide/qualify-name (format "make-%s" name)))
;;                 (:copier ,(elide/qualify-name (format "copy-%s" name))))
;;       ,@fields)))

;; (defun elide/expand-import (module &rest names)
;;   (if (not module)
;;       (dolist (name names)
;;         (puthash name name elide/env))
;;     (dolist (name names)
;;       (puthash name
;;                (make-symbol (format "%s-%s" module name))
;;                elide/env))
;;     `((require ',module))))

;; (defun elide/expand-expr (expr)
;;   (let ((env-value (and (consp expr)
;;                         (symbolp (car expr))
;;                         (gethash (car expr) elide/env))))
;;     (cond
;;      ((and env-value
;;            (consp env-value))
;;       (apply (car env-value) (cdr expr)))
;;      ;; ((eq 'import (car-safe expr))
;;      ;;  (apply 'elide/expand-import (cdr expr)))
;;      ((eq 'quote (car-safe expr))
;;       (list expr))
;;      ((listp expr)
;;       (list (mapcan 'elide/expand-expr expr)))
;;      ((symbolp expr)
;;       (cond
;;        (env-value
;;         (list env-value))
;;        ((or (memq expr '(\` \, \,@))
;;             (and (fboundp expr)
;;                  (subrp (symbol-function expr))))
;;         (list expr))
;;        (t (list (elide/qualify-name expr)))))
;;      (t (list expr)))))

(defmacro elide/expand (form)
  (condition-case err
      (eval form)
    (error err)))

(defun elide/mapforms (forms &rest rest)
  (mapcar (lambda (form)
            (apply 'elide/mapform form rest))
          forms))

;; (defun elide/mapform (form &rest func-pairs)
;;   (setq form
;;         (cond
;;          ((vectorp form)
;;           (apply 'vector (apply 'elide/mapforms (append form nil) func-pairs)))
;;          ((consp form)
;;           (cons (apply 'elide/mapform (car form) func-pairs)
;;                 (and (cdr form) (apply 'elide/mapform (cdr form) func-pairs))))
;;          (t form)))
;;   (let ((pairs func-pairs))
;;     (while pairs
;;       (if (funcall (car pairs) form)
;;           (setq form (funcall (cadr pairs) form)
;;                 pairs nil)
;;         (setq pairs (cddr pairs)))))
;;   form)

(defvar elide/mapform-context nil)

(defun elide/mapcar-improper (func list)
  (assert (listp list))
  (let* (last-link result)
    (while (consp list)
      (let ((new-link (cons (funcall func (car list))
                            (and (not (listp (cdr list)))
                                 (funcall func (cdr list))))))
        (when last-link
          (setcdr last-link new-link))
        (unless result
          (setq result new-link))
        (setq last-link new-link))
      (setq list (cdr list)))
    result))

(jasmel-spec "elide/mapcar-improper"
  (jasmel-case "works"
    (jasmel-expect (elide/mapcar-improper '1+ '(0 1 . 2))
      :equal '(1 2 . 3))))

(defun elide/visit-list-exprs (fn form)
  (cond
   ((vectorp form)
    (mapc (lambda (subform)
            (elide/visit-list-exprs fn subform))
          form))
   ((consp form)
    (when (null (cdr (last form)))
      (funcall fn form)
      (mapc (lambda (subform)
              (elide/visit-list-exprs fn subform))
            form)))))

(defun* elide/mapform (form &rest rest
                            &key
                            pre-proc
                            pre-func
                            post-func
                            post-proc)
  "Traverses all the forms contained in FORM and returns a
transformed version of FORM.

If PRE-FUNC is given, it is called before recursing into each
form, and the value it returns is used in place of FORM.

If POST-FUNC is given, it is called on the value build by
recusing into FORM, and the value is returns is the result of
this function."
  (when pre-proc
    (funcall pre-proc form))
  (when pre-func
    (setq form (funcall pre-func form)))
  (flet ((recur (form) (apply 'elide/mapform form rest)))
    (setq form
          (cond
           ((vectorp form)
            (apply 'vector (mapcar 'recur form)))
           ((consp form)
            (elide/mapcar-improper 'recur form))
           (t form))))
  (when post-func
    (setq form (funcall post-func form)))
  (when post-proc
    (funcall post-proc form))
  form)

(jasmel-spec "elide/mapform"
  (labels ((do-post-quote
            (form)
            (elide/mapform form
                           :post-func (lambda (form) `(quote ,form)))))
    (jasmel-case "works"
      (jasmel-expect (do-post-quote [a b c])
        :equal ''['a 'b 'c])
      (jasmel-expect (do-post-quote '(a b c))
        :equal ''('a 'b 'c))
      (jasmel-expect (do-post-quote '(a b . c))
        :equal ''('a 'b . 'c))
      (jasmel-expect (elide/mapform [0 1 2]
                                    :post-func
                                    (lambda (form)
                                      (if (integerp form)
                                          (1+ form)
                                        form)))
        :equal [1 2 3])
      (jasmel-expect (elide/mapform '(0 1 . 2)
                                    :post-func
                                    (lambda (form)
                                      (if (integerp form)
                                          (1+ form)
                                        form)))
        :equal '(1 2 . 3)))))

;; (defun elide/mapform-test ()
;;   (flet ((do-post-quote
;;           (form)
;;           (elide/mapform form
;;                          :post-func (lambda (form) `(quote ,form)))))
;;     (assert (equal ''['a 'b 'c]
;;                    (do-post-quote [a b c])))
;;     (assert (equal ''('a 'b 'c)
;;                    (do-post-quote '(a b c))))
;;     (assert (equal ''('a 'b . 'c)
;;                    (do-post-quote '(a b . c))))
;;     (assert (equal [1 2 3]
;;                    (elide/mapform [0 1 2]
;;                                   :post-func
;;                                   (lambda (form)
;;                                     (if (integerp form)
;;                                         (1+ form)
;;                                       form)))))
;;     (assert (equal '(1 2 . 3)
;;                    (elide/mapform '(0 1 . 2)
;;                                   :post-func
;;                                   (lambda (form)
;;                                     (if (integerp form)
;;                                         (1+ form)
;;                                       form)))))))

;; (elide/mapform-test)

;; (defmacro elide/cond-let* (&rest forms)
;;   (with-gensyms (result-var)
;;     `(let (,result-var)
;;        (progn
;;          (cond)
;;          result-var))
;;     ))

;; (defun elide/mapexprs (exprs &rest rest)
;;   (mapcar (lambda (expr) (apply 'elide/mapexpr expr rest))
;;           exprs))

;; (put 'defvar 'elide/mapexpr-function
;;      'elide/defvar-mapexpr-function)
;; (put 'defconst 'elide/mapexpr-function
;;      'elide/defvar-mapexpr-function)

;; (defun elide/defvar-mapexpr-function (form &rest rest
;;                                            &key defined-name-func)
;;   (if (not (symbolp (cadr form)))
;;       (apply 'elide/defvar-mapexpr-function form rest)
;;     (list* head
;;            (funcall defined-name-func (cadr expr))
;;            (cddr expr))))

;; (put 'defun 'elide/mapexpr-function
;;      'elide/defun-mapexpr-function)
;; (put 'defmacro 'elide/mapexpr-function
;;      'elide/defun-mapexpr-function)

;; (defun elide/defun-mapexpr-function (form &rest rest
;;                                           &key
;;                                           defined-name-func
;;                                           local-name-func)
;;   (if (not (and (memq head '(defun defmacro))
;;                 (symbolp (cadr expr))))
;;       (apply 'elide/defvar-mapexpr-function form rest)
;;     (list* head
;;            (funcall defined-name-func (cadr expr))
;;            (mapcar local-name-func (caddr expr))
;;            (cdddr expr))))

;; (defun elide/default-mapexpr-function (form &rest rest)
;;   (lambda (expr)
;;     (cond
;;      ((atom expr)
;;       (funcall atom-func expr))
;;      (t
;;       (let ((head (and (symbolp (car expr))
;;                        (car expr))))
;;         (cond
;;          ;; ((and (memq head '(defconst defvar))
;;          ;;       (symbolp (cadr expr)))
;;          ;;  (list* head
;;          ;;         (funcall defined-name-func (cadr expr))
;;          ;;         (cddr expr)))
;;          ;; ((and (memq head '(defun defmacro))
;;          ;;       (symbolp (cadr expr)))
;;          ;;  (list* head
;;          ;;         (funcall defined-name-func (cadr expr))
;;          ;;         (mapcar local-name-func (caddr expr))
;;          ;;         (cdddr expr)))
;;          ((memq head '(let let*))
;;           (list* head
;;                  (mapcar (lambda (binding)
;;                            (if (symbolp binding)
;;                                (funcall local-name-func binding)
;;                              (list
;;                               (funcall local-name-func (car binding))
;;                               (cadr binding))))
;;                          (cadr expr))
;;                  (cddr expr)))
;;          ((eq head 'function)
;;           (if (and (consp (cadr expr))
;;                    (eq 'lambda (caadr expr)))
;;               (let* ((lambda-form (cadr expr))
;;                      (lambda-args (cadr lambda-form))
;;                      (lambda-body (cddr lambda-form)))
;;                 `(function (lambda ,(mapcar local-name-func lambda-args)
;;                              ,@lambda-body)))
;;             expr))
;;          ((eq head 'defalias)
;;           (if (eq 'quote (car-safe (cadr expr)))
;;               (list* head
;;                      (list 'quote (funcall defined-name-func (cadadr expr)))
;;                      (cddr expr))))
;;          (t
;;           expr)))))))

;; (defun* elide/mapexpr (expr &rest rest
;;                             &key
;;                             (defined-name-func 'identity)
;;                             (local-name-func 'identity)
;;                             (atom-func 'identity)
;;                             (quote-func 'identity))
;;   (elide/mapform
;;    expr
;;    :post-func
;;    (lambda (expr)
;;      (let ((head (car-safe expr)))
;;        (if (and (symbolp head)
;;                 (get head 'elide/mapexpr-function))
;;            (apply (get head 'elide/mapexpr-function) expr rest)
;;          (apply 'elide/default-mapexpr-function expr rest)))
;;      ;; (cond
;;      ;;  ((atom expr)
;;      ;;   (funcall atom-func expr))
;;      ;;  (t
;;      ;;   (let ((head (and (symbolp (car expr))
;;      ;;                    (car expr))))
;;      ;;     (cond
;;      ;;      ;; ((get head 'elide/mapexpr-function)
;;      ;;      ;;  (funcall (get head 'elide/mapexpr-function expr)))
;;      ;;      ((and (memq head '(defconst defvar))
;;      ;;            (symbolp (cadr expr)))
;;      ;;       (list* head
;;      ;;              (funcall defined-name-func (cadr expr))
;;      ;;              (cddr expr)))
;;      ;;      ((and (memq head '(defun defmacro))
;;      ;;            (symbolp (cadr expr)))
;;      ;;       (list* head
;;      ;;              (funcall defined-name-func (cadr expr))
;;      ;;              (mapcar local-name-func (caddr expr))
;;      ;;              (cdddr expr)))
;;      ;;      ((memq head '(let let*))
;;      ;;       (list* head
;;      ;;              (mapcar (lambda (binding)
;;      ;;                        (if (symbolp binding)
;;      ;;                            (funcall local-name-func binding)
;;      ;;                          (list
;;      ;;                           (funcall local-name-func (car binding))
;;      ;;                           (cadr binding))))
;;      ;;                      (cadr expr))
;;      ;;              (cddr expr)))
;;      ;;      ((eq head 'function)
;;      ;;       (if (and (consp (cadr expr))
;;      ;;                (eq 'lambda (caadr expr)))
;;      ;;           (let* ((lambda-form (cadr expr))
;;      ;;                  (lambda-args (cadr lambda-form))
;;      ;;                  (lambda-body (cddr lambda-form)))
;;      ;;             `(function (lambda ,(mapcar local-name-func lambda-args)
;;      ;;                          ,@lambda-body)))
;;      ;;         expr))
;;      ;;      ((eq head 'defalias)
;;      ;;       (if (eq 'quote (car-safe (cadr expr)))
;;      ;;           (list* head
;;      ;;                  (list 'quote (funcall defined-name-func (cadadr expr)))
;;      ;;                  (cddr expr))))
;;      ;;      (t
;;      ;;       expr)))))
;;      )))

;; (defun elide/add-name (name table)
;;   (puthash name t table))

;; (defun elide/find-names (exprs)
;;   (let ((table (make-hash-table :test 'eq)))
;;     (elide/mapexprs exprs
;;                      :defined-name-func
;;                      (lambda (name)
;;                        (puthash name
;;                                 (intern (format "%s-%s"
;;                                                 elide/module-name
;;                                                 name))
;;                                 table)))
;;     table))

;; (defun elide/replace-names (exprs table)
;;   (elide/mapexprs exprs
;;                   :atom-func
;;                   (lambda (atom)
;;                     (if (or (not atom)
;;                             (not (symbolp atom))
;;                             (not (gethash atom table)))
;;                         atom
;;                       (gethash atom table)))
;;                   :defined-name-func
;;                   (lambda (name)
;;                     (gethash name table))))

(defun elide/strip-strings (expr)
  (elide/mapexpr expr
                 :atom-func (lambda (atom)
                              (if (and (stringp atom)
                                       (string-match-p " " atom))
                                  "..."
                                atom))
                 :quote-func (lambda (q)
                               (list* 'quote
                                      (mapcar 'elide/strip-strings (cdr q))))))

;; (defun elide/show-preview ()
;;   (interactive)
;;   (let (form)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (progn (setq form (read (current-buffer)))
;;                     (not (eq 'elide/module (car-safe form))))))
;;     (setq form form)
;;     (let ((inhibit-read-only t))
;;       (pop-to-buffer "*Elide/Preview*")
;;       (setq buffer-read-only t)
;;       (delete-region (point-min) (point-max))
;;       (insert ";; Expanded:")
;;       (cl-prettyprint (elide/strip-strings (macroexpand form)))
;;       (unless elide/expand-macros
;;         (insert "\n\n;; Expanded more:")
;;         (cl-prettyprint (macroexpand-all form)))
;;       (goto-char (point-min))
;;       (emacs-lisp-mode))))

(defvar elide/annotating nil)

(defconst elide/suppress-after-save-hook nil)

(defun elide/after-save-hook ()
  (unless elide/suppress-after-save-hook
    (condition-case err
        (progn
          (elide/update-buffer)
          (let ((elide/suppress-after-save-hook t))
            (save-buffer)))
      (error (display-warning
              (car err)
              (format "elide/after-save-hook: %s: %s" (buffer-name) (cadr err))
              :error)))))

(defun elide/map-forms-1 (fn)
  "For each top-level form in the current buffer, calls (FN form
form-beginning form-end form-symbols.  The arguments are:

- form: The parsed form itself.

- form-beginning: The buffer offset of the start of the form.

- form-end: The buffer offset of the end of the form.

- form-symbols: The symbol occurrences in the form, in backwards
  order.

The values in form-symbols are three-element lists containing the
symbol name, its start offset, and its end offset."
  (save-excursion
    (goto-char (point-min))
    (let ((read-with-symbol-positions t)
          ;;(inhibit-read-only t)
          (alt-obarray (make-vector 51 nil))
          read-symbol-positions-list)
      (condition-case nil
          (while t
            (let* ((read-beginning (point))
                   ;; (form (read (current-buffer)))
                   (form (elide/mapform
                          ;; Read form with symbols interned into an
                          ;; alternate obarray.
                          (let ((obarray alt-obarray))
                            (read (current-buffer)))
                          :post-func
                          ;; For symbols in alt-obarray that exist in
                          ;; obarray, replace them with the version
                          ;; from obarray.
                          (lambda (form)
                            (if (and (symbolp form)
                                     (intern-soft (symbol-name form)))
                                (intern (symbol-name form))
                              form))))
                   (form-end (point))
                   (form-end-marker (copy-marker form-end t))
                   (form-beginning (save-excursion
                                     (backward-sexp)
                                     (point)))
                   form-symbols)
              (dolist (pair read-symbol-positions-list)
                (let* ((symbol-name (symbol-name (car pair)))
                       (symbol-beginning (+ read-beginning (cdr pair)))
                       (symbol-end (+ symbol-beginning (length symbol-name))))
                  (push (list symbol-name
                              symbol-beginning
                              symbol-end)
                        form-symbols)))
              (funcall fn form form-beginning form-end form-symbols)
              (goto-char form-end-marker)))
        (end-of-file)))))

(defun elide/map-forms (fn)
  "For each top-level form in the current buffer, calls (FN form
form-beginning form-end form-symbols.  The arguments are:

- form: The parsed form itself.

- form-beginning: The buffer offset of the start of the form.

- form-end: The buffer offset of the end of the form.

- form-symbols: The symbol occurrences in the form, in backwards
  order.

The values in form-symbols are three-element lists containing the
symbol name, its start offset, and its end offset."
  (save-excursion
    (goto-char (point-min))
    (let ((read-with-symbol-positions t)
          ;;(alt-obarray (make-vector 51 nil))
          read-symbol-positions-list)
      (condition-case nil
          (while t
            (let* ((read-beginning (point))
                   (form (read (current-buffer)))
                   ;; Too slow!
                   ;; (form (elide/mapform
                   ;;        ;; Read form with symbols interned into an
                   ;;        ;; alternate obarray.
                   ;;        (let ((obarray alt-obarray))
                   ;;          (read (current-buffer)))
                   ;;        :post-func
                   ;;        ;; For symbols in alt-obarray that exist in
                   ;;        ;; obarray, replace them with the version
                   ;;        ;; from obarray.
                   ;;        (lambda (form)
                   ;;          (if (and (symbolp form)
                   ;;                   (intern-soft (symbol-name form)))
                   ;;              (intern (symbol-name form))
                   ;;            form))))
                   (form-end (point))
                   (form-end-marker (copy-marker form-end t))
                   (form-beginning (save-excursion
                                     (backward-sexp)
                                     (point)))
                   form-symbols)
              (dolist (pair read-symbol-positions-list)
                (let* ((symbol-name (symbol-name (car pair)))
                       (symbol-beginning (+ read-beginning (cdr pair)))
                       (symbol-end (+ symbol-beginning (length symbol-name))))
                  (push (list symbol-name
                              symbol-beginning
                              symbol-end)
                        form-symbols)))
              (funcall fn form form-beginning form-end form-symbols)
              (goto-char form-end-marker)))
        (end-of-file)))))

;; (defun elide/update-forms (fn)
;;   (elide/map-forms
;;    (lambda (form form-beginning form-end form-symbols)
;;      (save-excursion
;;        (goto-char form-beginning)))))

(defun elide/update-buffer ()
  (unless (buffer-modified-p)
    (revert-buffer))
  (elide-mode))

;; (defun* elide/map-property-regions (fn beg end &key object property)
;;   (let ((region-start beg))
;;     (while (< point end)
;;       (let* ((region-end (or (if property
;;                                  (next-single-property-change region-start property object end)
;;                                (next-property-change region-start object end))
;;                              end)))
;;         (funcall fn region-start region-end)
;;         (setq region-start region-end)))))

(defun elide/export-symbol ()
  (interactive)
  (let ((name (thing-at-point 'symbol)))
    (if (gethash name elide/exported-names)
        (progn
          (message "Un-exporting symbol %s" name)
          (remhash name elide/exported-names))
      (message "Exporting symbol %s" name)
      (puthash name t elide/exported-names)))
  (set-buffer-modified-p t)
  ;;(elide/annotate-symbols)
  )

(defun elide/update-buffers ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'elide-mode)
        (elide/update-buffer)))))

;; (defun elide/emacs-lisp-mode-hook ()
;;   (elide-mode))

;; (add-hook 'emacs-lisp-mode-hook 'elide/emacs-lisp-mode-hook)

;; (defun elide/lcs (s1 s2)
;;   (elide/lcs-1 s1 0 (length s1) s2 0 (length s2)))

;; (defun elide/lcs-1 (s1 i1 j1 s2 i2 j2)
;;   (cond
;;    ((or (= i1 j1)
;;         (= i2 j2)))
;;    ((string= (substring s1 (1- j1) j1)
;;              (substring s2 (1- j2) j2)))))

;; (defun elide/string-diff (s1 s2)
;;   (if (string= s1 s2)
;;       nil
;;     (let ((prefix-len 0)
;;           (suffix-len 0))
;;       (while (and (<= prefix-len (length s1))
;;                   (<= prefix-len (length s2))
;;                   (string= (substring s1 0 prefix-len)
;;                            (substring s2 0 prefix-len)))
;;         (incf prefix-len))
;;       (while (and (<= suffix-len (length s1))
;;                   (<= suffix-len (length s2))
;;                   (string= (substring s1 (- (length s1) suffix-len))
;;                            (substring s2 (- (length s2) suffix-len))))
;;         (incf suffix-len))
;;       (format "%s[[[%s]]][[[%s]]]%s"
;;               (substring s1 0 prefix-len)
;;               (substring s1 prefix-len (- (length s1) suffix-len))
;;               (substring s2 prefix-len (- (length s2) suffix-len))
;;               (substring s1 (- (length s1) suffix-len))))))

(defun elide/map-forms-speed-test ()
  (elide/map-forms (lambda (&rest _args) nil)))

(defun elide/test ()
  ;; (dotimes (i 100)
  ;;   (elide/mapatoms-speed-test))
  (catch 'error
    (dolist (basename '( ;;"~/elisp/elide.el"
                         "~/elisp/example.el"
                         "~/elisp/example2.el"))
      (let ((test-file (expand-file-name basename)))
        (with-temp-buffer
          (insert-file-contents test-file)
          (let ((orig-content (buffer-string)))
            ;;(elide-mode)
            (format-decode-buffer 'elide-mode)
            (elide/map-forms-speed-test)
            ;;(emacs-lisp-mode)
            (format-encode-buffer 'elide-mode)
            (unless (equal orig-content (buffer-string))
              (message "Content of %s is not invariant!" test-file)
              (setq result nil)
              (let ((temp-file (make-temp-file "mangled" nil ".el")))
                (write-file temp-file)
                (call-process "gvimdiff" nil nil nil test-file temp-file)
                (delete-file temp-file))
              (throw 'error nil))))))
    t))

(defun elide/instrument-symbols ()
  (require 'elp)
  (setq elp-function-list
        '(string-match intern))
  (mapatoms
   (lambda (sym)
     (when (and (fboundp sym)
                (functionp (symbol-function sym))
                (string-match-p 
                 "^elide[/-]"
                 (symbol-name sym))
                (not (memq sym '(elide/instrument-symbols
                                 elide/mapform
                                 elide/decompose-symbol
                                 elide/mapcar-improper
                                 elide/map-forms
                                 elide/dummy-func))))
       (push sym elp-function-list))))
  (elp-instrument-list))

(jasmel-spec "elide-mode"
  (jasmel-case "large test"
    (let ((elide/elide-mode-debug t))
      (elide/instrument-symbols)
      (elp-reset-all)
      (elp-set-master 'elide/test)
      ;;(elp-set-master 'elide/update-namespaces)
      ;;(elp-set-master 'elide/decompose-symbol)
      (let ((start-time (float-time))
            end-time)
        (when (elide/test)
          (elide/update-buffers))
        (setq end-time (float-time))
        (when (> (- end-time start-time) 0.5)
          (elp-results)))
      (elp-restore-all))))


(provide 'elide-mode)

;; Local Variables:
;; no-byte-compile: t
;; End:
