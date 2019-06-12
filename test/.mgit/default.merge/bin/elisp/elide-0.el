;; -*- no-byte-compile: t -*-
;; -*- lexical-binding: t; -*-
(require 'cl)

(defconst elide-mode-debug nil)
(defconst elide-expand-macros t)
(defvar elide-show-preview t)

(defvar elide-content-beginning)
(defvar elide-content-end)
(defconst elide-annotation-face 'shadow)
(defconst elide-protected-face 'shadow)

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
   '(elide-search-annotation . (0 elide-annotation-face prepend))
   `(,elide-protect-pattern . (0 elide-protected-face prepend))
   lisp-font-lock-keywords-2))

(defun elide-search-annotation (limit)
  ;; (let ((change (next-single-property-change (point) 'elide-annotation nil limit)))
  ;;   (when change
  ;;     (goto-char change)
  ;;     (looking-at "...............")
  ;;     ;;(set-match-data (list change (+ 15 change)))
  ;;     )
  ;;   )
  (re-search-forward elide-annotation-pattern limit t))

(define-derived-mode elide-mode emacs-lisp-mode "Elide"
  "Emacs Lisp IDE major mode `foo'."
  (setq font-lock-defaults
        (list* '(elide-font-lock-keywords)
               (cdr font-lock-defaults)))
  (add-hook 'before-save-hook 'elide-before-save-hook nil 'local)
  (add-hook 'after-save-hook 'elide-after-save-hook nil 'local)
  (add-to-invisibility-spec 'elide-mode)
  (make-local-variable 'elide-content-beginning)
  (make-local-variable 'elide-content-end)
  (add-hook 'change-major-mode-hook
            'elide-change-major-mode-hook)
  (unless buffer-read-only
    (elide-annotate-buffer)))

(defmacro elide-save-modified (&rest body)
  `(let ((was-modified (buffer-modified-p))
         (inhibit-read-only t))
     (unwind-protect
         (save-excursion ,@body)
       (set-buffer-modified-p was-modified))))

(defun elide-change-major-mode-hook ()
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
  (elide-add-boilerplate)
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

(defun elide-add-boilerplate ()
  (let ((module-name (if (and (buffer-file-name)
                              (string-match "\\([^/]+\\)\\.el$"
                                            (buffer-file-name)))
                         (match-string 1 (buffer-file-name))
                       "anonymous")))
    (elide-save-modified
     (remove-overlays nil nil 'elide-mode t)
     (save-excursion
       (goto-char (point-min))

       ;; Skip over local variables.
       (if (looking-at ";; *-\\*-")
           (forward-line)
         (insert ";; -*- mode: elide; lexical-binding: t -*-\n"))

       (elide-delete-protected-lines "top-boilerplate")
       ;; (let ((boilerplate-begin (point)))
       ;;   ;; Delete old top boilerplate.
       ;;   (when (looking-at ";; begin elide top boilerplate$")
       ;;     (re-search-forward "^;; end elide top boilerplate$")
       ;;     (forward-line)
       ;;     (delete-region boilerplate-begin (point)))

       (elide-insert-protected "(require 'elide)" "top-boilerplate")
       (elide-insert-protected (format "(elide-module %s" module-name) "top-boilerplate")

       ;; (condition-case nil
       ;;     (forward-sexp)
       ;;   (scan-error (goto-char (point-max))))

       ;; Add new top boilerplate.
       ;;   (insert ";; begin elide top boilerplate\n"
       ;;           "(require 'elide)\n"
       ;;           "(elide-module-wrapper " module-name "\n"
       ;;           ";; end elide top boilerplate\n")
       ;;   (elide-add-boilerplate-overlay
       ;;    boilerplate-begin (point)
       ;;    'before-string (propertize
       ;;                    (format ";;; module %s" module-name)
       ;;                    'face 'shadow)))
       ;; (setq elide-content-beginning (point-marker))

       ;; (if (re-search-forward "^;; begin elide bottom boilerplate$" nil 'noerror)
       ;;     ;; Delete bottom boilerplate.
       ;;     (let ((boilerplate-begin (match-beginning 0)))
       ;;       (re-search-forward "^;; end elide bottom boilerplate$")
       ;;       (forward-line)
       ;;       (delete-region boilerplate-begin (point)))
       ;;   (goto-char (point-max)))

       ;; Insert new bottom boilerplate.
       (unless (bolp)
         (insert "\n"))
       (unless (elide-delete-protected-lines "bottom-boilerplate")
         (goto-char (point-max)))
       (elide-insert-protected ") ;; elide-module" "bottom-boilerplate")
       ;; (setq elide-content-end (point-marker))
       ;; (insert ";; begin elide bottom boilerplate\n"
       ;;         ")\n"
       ;;         ";; end elide bottom boilerplate\n")
       ;; (elide-add-boilerplate-overlay
       ;;  (marker-position elide-content-end) (point)
       ;;  'after-string (propertize
       ;;                 (format ";;; end module %s" module-name)
       ;;                 'face 'shadow)
       ))))

(defun elide-add-boilerplate-overlay (begin end &rest extra-properties)
  (add-text-properties begin end '(read-only t))
  (let ((overlay (make-overlay begin end nil t))
        (props (append extra-properties
                       (list
                        'elide-mode t
                        'face 'shadow)
                       ;; (unless nil ;; elide-mode-debug
                       ;;   '(invisible elide-mode))
                       )))
    (while props
      (overlay-put overlay (car props) (cadr props))
      (setq props (cddr props)))))

(defvar elide-env)
(defvar elide-module-name)
(defvar elide-expansions nil)

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

(defun elide-update-buffer ()
  (elide-mode)
  (let ((elide-expansions t))
    (condition-case nil
        (progn (eval-buffer) t)
      (error nil))
    (elide-remove-annotations)
    (elide-annotate-defuns)
    ))

(defun elide-update-buffers ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'elide-mode)
        (elide-update-buffer)))))

(let ((elide-mode-debug t))
  (elide-update-buffers))

(provide 'elide)

;; Local Variables:
;; no-byte-compile: t
;; End:
