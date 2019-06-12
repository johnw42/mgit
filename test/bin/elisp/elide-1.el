;; -*- no-byte-compile: t -*-
;; -*- lexical-binding: t; -*-
(require 'cl)

(defconst elide-mode-debug nil)
(defconst elide-expand-macros t)
(defvar elide-show-preview t)

(defvar elide-content-beginning)
(defvar elide-content-end)
(defvar elide-namespace)
(defvar elide-env)
;; (defvar elide-module-name)
;; (defvar elide-expansions nil)
(defconst elide-annotation-face 'shadow)
(defconst elide-protected-face 'shadow)
(defconst elide-local-name-face 'bold)

(defun elide-protect-marker (&optional tag)
  (propertize (concat " ;;elide-protect;;" tag) 'invisible 'elide-mode))

(defconst elide-protect-marker
  (elide-protect-marker))

(defun elide-protect-marker-pattern (tag)
  (concat (regexp-quote elide-protect-marker)
          (if tag
              (regexp-quote tag)
            "[-a-z]*") "$"))

(defconst elide-protect-marker-pattern
  (elide-protect-marker-pattern nil))

(defconst elide-protect-pattern
  (concat "^.*" elide-protect-marker-pattern))

(defconst elide-annotation-marker
  (concat (propertize ";;elide-note" 'invisible 'elide-mode)
          ";; "))

(defconst elide-annotation-marker-pattern
  (concat (regexp-quote elide-annotation-marker)))

(defconst elide-annotation-pattern
  (concat "^[^;\n]*" elide-annotation-marker-pattern "[^\n]*"))

(defconst elide-font-lock-keywords
  (list*
   ;;'(elide-search-local-name . (0 elide-local-name-face prepend))
   ;; '(elide-search-annotation . (0 elide-annotation-face prepend))
   ;; `(,elide-protect-pattern . (0 elide-protected-face prepend))
   lisp-font-lock-keywords-2))

(defun elide-search-local-name (limit)
  (let* ((search-start (point))
         (props-here (text-properties-at search-start))
         (prop-start (if (plist-get props-here :elide-local-symbol)
                         search-start
                       (next-single-property-change search-start :elide-local-symbol nil limit)))
         (prop-end (and prop-start
                        (or (next-single-property-change prop-start
                                                         :elide-local-symbol
                                                         nil limit)
                            limit))))
    (when prop-end
      ;;(message "elide-search-local-name %s-%s %s-%s" search-start limit prop-start prop-end)
      (set-match-data (list prop-start prop-end))
      (goto-char prop-end)
      t)))

(defun elide-search-annotation (limit)
  ;; (let ((change (next-single-property-change (point) 'elide-annotation nil limit)))
  ;;   (when change
  ;;     (goto-char change)
  ;;     (looking-at "...............")
  ;;     ;;(set-match-data (list change (+ 15 change)))
  ;;     )
  ;;   )
  (re-search-forward elide-annotation-pattern limit t))

(defconst elide-mode-hooks '((change-major-mode-hook elide-change-major-mode-hook)
                             (before-save-hook elide-before-save-hook)
                             (after-save-hook elide-after-save-hook)
                             (after-change-functions elide-after-change-function)
                             (after-revert-hook elide-after-revert-hook)))

(defconst elide-invisibility-spec '(elide-local-symbol))

(define-derived-mode elide-mode emacs-lisp-mode "Elide"
  "Emacs Lisp IDE major mode `foo'."
  (setq font-lock-defaults
        (list* '(elide-font-lock-keywords)
               (cdr font-lock-defaults)))
  (dolist (hook elide-mode-hooks)
    (add-hook (car hook) (cadr hook) nil 'local))
  (dolist (spec elide-invisibility-spec)
    (add-to-invisibility-spec spec))
  (make-local-variable 'elide-content-beginning)
  (make-local-variable 'elide-content-end)
  (make-local-variable 'elide-namespace)
  (make-local-variable 'elide-env)
  (unless buffer-read-only
    (elide-annotate-buffer)))

(defvar elide-mode-alt-syntax-table
  (let ((table (make-syntax-table elide-mode-syntax-table)))

    table))
(modify-syntax-entry (cons 0 255) "w" elide-mode-alt-syntax-table)

(defmacro elide-save-modified (&rest body)
  `(let ((was-modified (buffer-modified-p))
         (inhibit-read-only t))
     (unwind-protect
         (save-excursion ,@body)
       (set-buffer-modified-p was-modified))))

(defun elide-change-major-mode-hook ()
  (dolist (hook elide-mode-hooks)
    (remove-hook (car hook) (cadr hook) 'local))
  (dolist (spec elide-invisibility-spec)
    (remove-from-invisibility-spec spec))
  (elide-save-modified
   (let (ann-beginning
         (ann-end (point-min)))
     (while (setq ann-beginning
                  (next-single-property-change
                   ann-end 'elide-annotation))
       (setq ann-end (or (next-single-property-change
                          ann-beginning
                          'elide-annotations)
                         (point-max)))
       (when (get-text-property ann-beginning 'elide-annotation)
         (goto-char ann-beginning)
         (set-text-properties ann-beginning ann-end nil)
         (setq ann-end ann-beginning))))))

(defun elide-remove-annotations ()
  (let ((case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward elide-annotation-marker-pattern
                                nil 'noerror)
        (forward-line)
        (delete-region (match-beginning 0) (point))))))


(defun elide-insert-annotation (text)
  (assert (bolp))
  (let ((old-point (point)))
    (insert (with-temp-buffer
              (insert text "\n")
              (goto-char (point-min))
              (while (not (eobp))
                (insert elide-annotation-marker)
                (forward-line))
              (buffer-string)))
    (elide-mark-annotation old-point (1- (point)))))

(defun elide-mark-annotation (beginning end)
  (add-text-properties beginning end
                       (list 'elide-annotation t
                             'read-only t
                             'front-sticky t
                             )))

(defun elide-find-annotations ()
  (elide-save-modified
   (let ((case-fold-search nil))
     (goto-char (point-min))
     (while (re-search-forward elide-annotation-marker-pattern
                               nil 'noerror)
       (let ((inhibit-read-only t))
         (delete-region (match-beginning 0)
                        (match-end 0))
         (insert elide-annotation-marker)
         (end-of-line)
         (elide-mark-annotation (match-beginning 0) (point)))))))

(defun elide-map-sexps (f)
  "For each top-level form in the module, call F.  The arguments
to F are BEG, a marker at the beginng on the form, END, a marker
at the end of the form, and FORM, the parsed form itself."
  (let (result beg end)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^(elide-module ")
      (forward-sexp)
      (condition-case nil
          (while t
            (forward-sexp)
            (setq end (point-marker))
            (backward-sexp)
            (setq beg (point-marker))
            (goto-char end)
            (funcall f beg end
                     (read (buffer-substring beg end))))
        (scan-error nil)))))

(defun elide-annotate-defuns ()
  (when (listp elide-expansions)
    (save-excursion
      (elide-map-sexps
       (lambda (sexp-begin sexp-end sexp)
         (goto-char sexp-end)
         (forward-line)
         (elide-insert-annotation
          ;;(cl-prettyprint (elide-strip-strings (pop expansions)))
          (format "%s" (with-temp-buffer (cl-prettyprint (elide-strip-strings
                                                          (pop elide-expansions)))
                                         (buffer-string)))
          ;;(format "annotation for %s" (if (listp sexp) (car sexp) sexp))
          ))))))

(defun elide-annotate-buffer ()
  ;;(elide-add-boilerplate)
  ;;(elide-find-annotations)
  )

(defun elide-delete-line ()
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (forward-line)
      (delete-region beg (point)))))

(defun elide-delete-protected-lines (tag)
  (let ((pattern (elide-protect-marker-pattern tag)))
    (if (not (re-search-forward pattern nil t))
        nil
      (save-excursion
        (elide-delete-line)
        (while (re-search-forward pattern nil t)
          (elide-delete-line)))
      t)))

(defun elide-insert-protected (text tag)
  (assert (bolp))
  (let ((marker (elide-protect-marker tag))
        (old-point (point)))
    (insert (with-temp-buffer
              (insert text "\n")
              (goto-char (point-min))
              (while (not (eobp))
                (end-of-line)
                (insert marker)
                (forward-line))
              (buffer-string)))
    (elide-mark-annotation old-point (1- (point)))))

(defun elide-make-default-env ()
  (let ((env (make-hash-table :test 'eq)))
    (dolist (def '((import . (elide-expand-import))
                   (defstruct . (elide-expand-defstruct))))
      (puthash (car def) (cdr def) env))
    env))

;; (defun elide-qualify-name (name)
;;   (let ((qname (make-symbol (format "%s-%s" elide-module-name name))))
;;     (puthash name qname elide-env)
;;     qname))

;; (defun elide-expand-defstruct (name &rest fields)
;;   `((defstruct (,(elide-qualify-name name)
;;                 (:constructor ,(elide-qualify-name (format "make-%s" name)))
;;                 (:copier ,(elide-qualify-name (format "copy-%s" name))))
;;       ,@fields)))

;; (defun elide-expand-import (module &rest names)
;;   (if (not module)
;;       (dolist (name names)
;;         (puthash name name elide-env))
;;     (dolist (name names)
;;       (puthash name
;;                (make-symbol (format "%s-%s" module name))
;;                elide-env))
;;     `((require ',module))))

;; (defun elide-expand-expr (expr)
;;   (let ((env-value (and (consp expr)
;;                         (symbolp (car expr))
;;                         (gethash (car expr) elide-env))))
;;     (cond
;;      ((and env-value
;;            (consp env-value))
;;       (apply (car env-value) (cdr expr)))
;;      ;; ((eq 'import (car-safe expr))
;;      ;;  (apply 'elide-expand-import (cdr expr)))
;;      ((eq 'quote (car-safe expr))
;;       (list expr))
;;      ((listp expr)
;;       (list (mapcan 'elide-expand-expr expr)))
;;      ((symbolp expr)
;;       (cond
;;        (env-value
;;         (list env-value))
;;        ((or (memq expr '(\` \, \,@))
;;             (and (fboundp expr)
;;                  (subrp (symbol-function expr))))
;;         (list expr))
;;        (t (list (elide-qualify-name expr)))))
;;      (t (list expr)))))

(defmacro elide-expand (form)
  (condition-case err
      (eval form)
    (error err)))

(defun elide-mapforms (forms &rest rest)
  (mapcar (lambda (form)
            (apply 'elide-mapform form rest))
          forms))

;; (defun elide-mapform (form &rest func-pairs)
;;   (setq form
;;         (cond
;;          ((vectorp form)
;;           (apply 'vector (apply 'elide-mapforms (append form nil) func-pairs)))
;;          ((consp form)
;;           (cons (apply 'elide-mapform (car form) func-pairs)
;;                 (and (cdr form) (apply 'elide-mapform (cdr form) func-pairs))))
;;          (t form)))
;;   (let ((pairs func-pairs))
;;     (while pairs
;;       (if (funcall (car pairs) form)
;;           (setq form (funcall (cadr pairs) form)
;;                 pairs nil)
;;         (setq pairs (cddr pairs)))))
;;   form)

(defun* elide-mapform (form &rest rest
                            &key
                            pre-func
                            post-func)
  (when pre-func
    (setq form (funcall pre-func form)))
  (setq form
        (cond
         ((vectorp form)
          (apply 'vector (apply 'elide-mapforms (append form nil) rest)))
         ((consp form)
          (cons (apply 'elide-mapform (car form) rest)
                (and (cdr form) (apply 'elide-mapform (cdr form) rest))))
         (t form)))
  (when post-func
    (setq form (funcall post-func form)))
  form)

(defun elide-mapexprs (exprs &rest rest)
  (mapcar (lambda (expr) (apply 'elide-mapexpr expr rest))
          exprs))

(defun* elide-mapexpr (expr &rest rest
                            &key
                            (defined-name-func 'identity)
                            (local-name-func 'identity)
                            (atom-func 'identity)
                            (quote-func 'identity))
  (elide-mapform
   expr
   :post-func
   (lambda (expr)
     (cond
      ((atom expr)
       (funcall atom-func expr))
      (t
       (let ((head (and (symbolp (car expr))
                        (car expr))))
         (case head
           ((defconst defvar)
            (list* head
                   (funcall defined-name-func (cadr expr))
                   (cddr expr)))
           ((defun defmacro)
            (list* head
                   (funcall defined-name-func (cadr expr))
                   (mapcar local-name-func (caddr expr))
                   (cdddr expr)))
           ((let let*)
            (list* head
                   (mapcar (lambda (binding)
                             (if (symbolp binding)
                                 (funcall local-name-func binding)
                               (list
                                (funcall local-name-func (car binding))
                                (cadr binding))))
                           (cadr expr))
                   (cddr expr)))
           (function
            (if (and (consp (cadr expr))
                     (eq 'lambda (caadr expr)))
                (let* ((lambda-form (cadr expr))
                       (lambda-args (cadr lambda-form))
                       (lambda-body (cddr lambda-form)))
                  `(function (lambda ,(mapcar local-name-func lambda-args)
                               ,@lambda-body)))
              expr))
           (defalias
             (if (eq 'quote (car-safe (cadr expr)))
                 (list* head
                        (list 'quote (funcall defined-name-func (cadadr expr)))
                        (cddr expr))))
           (t
            expr))))))))

(defun elide-add-name (name table)
  (puthash name t table))

(defun elide-find-names (exprs)
  (let ((table (make-hash-table :test 'eq)))
    (elide-mapexprs exprs
                     :defined-name-func
                     (lambda (name)
                       (puthash name
                                (intern (format "%s-%s"
                                                elide-module-name
                                                name))
                                table)))
    table))

(defun elide-replace-names (exprs table)
  (elide-mapexprs exprs
                  :atom-func
                  (lambda (atom)
                    (if (or (not atom)
                            (not (symbolp atom))
                            (not (gethash atom table)))
                        atom
                      (gethash atom table)))
                  :defined-name-func
                  (lambda (name)
                    (gethash name table))))

(defmacro elide-module (name &rest body)
  (declare (indent (lambda (&rest args) 0)))
  (let* ((body (macroexpand-all body))
         (elide-env (elide-make-default-env))
         (elide-module-name name)
         (names-table (elide-find-names body))
         (top-expansions (elide-replace-names body names-table))
         (expansion
          `(progn
             :names
             ',(with-list-result r
                 (maphash (lambda (key value)
                            (push key r))
                          names-table))
             :expansion
             ,@top-expansions)))
    (when (eq t elide-expansions)
      (setq elide-expansions top-expansions))
    expansion))

(defun elide-before-save-hook ()
  ;; (condition-case err
  ;;     (progn
  ;;       (elide-annotate-buffer))
  ;;   (error (display-warning
  ;;           (car err)
  ;;           (format "elide-before-save-hook: %s: %s" (buffer-name) (cadr err))
  ;;           :error)))
  )

(defun elide-strip-strings (expr)
  (elide-mapexpr expr
                 :atom-func (lambda (atom)
                              (if (and (stringp atom)
                                       (string-match-p " " atom))
                                  "..."
                                atom))
                 :quote-func (lambda (q)
                               (list* 'quote
                                      (mapcar 'elide-strip-strings (cdr q))))))

;; (defun elide-show-preview ()
;;   (interactive)
;;   (let (form)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (progn (setq form (read (current-buffer)))
;;                     (not (eq 'elide-module (car-safe form))))))
;;     (setq form form)
;;     (let ((inhibit-read-only t))
;;       (pop-to-buffer "*Elide-Preview*")
;;       (setq buffer-read-only t)
;;       (delete-region (point-min) (point-max))
;;       (insert ";; Expanded:")
;;       (cl-prettyprint (elide-strip-strings (macroexpand form)))
;;       (unless elide-expand-macros
;;         (insert "\n\n;; Expanded more:")
;;         (cl-prettyprint (macroexpand-all form)))
;;       (goto-char (point-min))
;;       (emacs-lisp-mode))))

(defvar elide-annotating nil)

(defconst elide-suppress-after-save-hook nil)

(defun elide-after-save-hook ()
  (unless elide-suppress-after-save-hook
    (condition-case err
        (progn
          (elide-update-buffer)
          (let ((elide-suppress-after-save-hook t))
            (save-buffer)))
      (error (display-warning
              (car err)
              (format "elide-after-save-hook: %s: %s" (buffer-name) (cadr err))
              :error)))))

(defmacro import (&rest args))

(defun elide-map-forms (fn)
  (save-excursion
    (goto-char (point-min))
    (let ((read-with-symbol-positions t)
          (inhibit-read-only t)
          read-symbol-positions-list)
      (condition-case nil
          (while t
            (let* ((read-beginning (point))
                   (form (read (current-buffer)))
                   (form-end (point))
                   (form-beginning (save-excursion
                                     (backward-sexp)
                                     (point)))
                   form-symbols)
              (dolist (pair read-symbol-positions-list)
                (let* ((symbol (car pair))
                       (symbol-beginning (+ read-beginning (cdr pair)))
                       (symbol-end (+ symbol-beginning (length (symbol-name symbol)))))
                  (push (list symbol
                              symbol-beginning
                              symbol-end)
                        form-symbols)))
              (funcall fn form form-beginning form-end form-symbols)))
        (end-of-file)))))

(defun elide-update-forms (fn)
  (elide-map-forms
   (lambda (form form-beginning form-end form-symbols)
     (save-excursion
       (goto-char form-beginning)))))

(defun elide-update-buffer ()
  (let ((was-modified (buffer-modified-p)))
    (save-restriction
      (widen)
      (remove-text-properties (point-min) (point-max)
                              (list :elide-symbol nil
                                    :elide-form nil
                                    :elide-form-beginning nil
                                    :elide-form-end nil
                                    :elide-local-symbol nil
                                    'invisible nil
                                    'xinvisible nil))
      (setq elide-env (elide-make-default-env))
      (elide-map-forms
       (lambda (form form-beginning form-end form-symbols)
         (when (and (consp form)
                    (eq 'elide-namespace (car form)))
           (setq elide-namespace (cadr form)))
         (add-text-properties form-beginning form-end
                              (list ;;:elide-form-beginning form-beginning
                                    ;;:elide-form-end form-end
                                    ;;:elide-form form
                                    ))
         (dolist (pair form-symbols)
           (let* ((symbol (car pair))
                  (symbol-beginning (cadr pair))
                  (symbol-end (caddr pair)))
             (add-text-properties symbol-beginning symbol-end
                                  (list :elide-symbol symbol))))))
      (let ((symbol-pattern (format "%s-" (regexp-quote (symbol-name elide-namespace))))
            (prefix-length (1+ (length (symbol-name elide-namespace)))))
        (elide-map-forms
         (lambda (form form-beginning form-end form-symbols)
           (dolist (pair form-symbols)

             (let* ((symbol (car pair))
                    (symbol-name (symbol-name symbol))
                    (symbol-beginning (cadr pair))
                    (symbol-end (caddr pair))
                    (prefix-end (+ symbol-beginning prefix-length)))
               (when (string-match-p symbol-pattern symbol-name)
                 (add-text-properties prefix-end symbol-end
                                      (list :elide-local-symbol t
                                            :elide-namespace elide-namespace
                                            'font-lock-face 'bold))
                 (add-text-properties
                  symbol-beginning
                  prefix-end
                  (list 'xinvisible 'elide-local-symbol
                        'font-lock-face 'shadow
                        ;;'syntax-table '(1 . ?-)
                        'syntax-table (string-to-syntax "w")
                        ;;'syntax-table 'elide-mode-alt-syntax-table
                        ;;'yank-handler '(nil "" nil nil)
                        )))))))))
    (set-buffer-modified-p was-modified)))

;; (defun* elide-map-property-regions (fn beg end &key object property)
;;   (let ((region-start beg))
;;     (while (< point end)
;;       (let* ((region-end (or (if property
;;                                  (next-single-property-change region-start property object end)
;;                                (next-property-change region-start object end))
;;                              end)))
;;         (funcall fn region-start region-end)
;;         (setq region-start region-end)))))

(defun elide-after-change-function (beg end old-len)
  ;; (message "elide-after-change-function")
  ;; (when (and (zerop old-len)
  ;;            (< beg end)
  ;;            (eq 'elide-local-symbol (plist-get (text-properties-at end) 'invisible))
  ;;            )
  ;;   (let* ((invisible-end (next-single-property-change end 'invisible))
  ;;          (beg (and invisible-end
  ;;                    (save-excursion
  ;;                      (goto-char end)
  ;;                      (and (not (zerop (skip-syntax-backward "w_")))
  ;;                           (max beg (point)))))))
  ;;     (when beg
  ;;       (let ((invisible-text (buffer-substring end invisible-end)))
  ;;         (save-excursion
  ;;           (put-text-property beg end :elide-local-symbol t)
  ;;           (delete-region end invisible-end)
  ;;           (goto-char beg)
  ;;           (insert invisible-text)
  ;;           )))))
  )

(defun elide-after-revert-hook ()
  (set-text-properties (point-min) (point-max) nil)
  (elide-update-buffer))

(defun elide-update-buffers ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'elide-mode)
        (elide-update-buffer)))))

;; (defun elide-emacs-lisp-mode-hook ()
;;   (elide-mode))

;; (add-hook 'emacs-lisp-mode-hook 'elide-emacs-lisp-mode-hook)

(let ((elide-mode-debug t))
  (elide-update-buffers))



(provide 'elide)

;; Local Variables:
;; no-byte-compile: t
;; End:
