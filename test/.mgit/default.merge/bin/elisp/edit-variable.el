;; -*- no-byte-compile: t -*-

(require 'thingatpt)


(defvar edit-variable-history-list nil)

(defvar edit-variable-buffer nil)
(put 'edit-variable-buffer 'permanent-local t)

(defvar edit-variable-symbol nil)
(put 'edit-variable-symbol 'permanent-local t)

(defun edit-variable-save-buffer ()
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated.")
  (revert-buffer))


(defun my-eval-and-kill-buffer ()
  "Evaluate the contents of the buffer, then kill it."
  (interactive)
  (eval-buffer)
  (kill-buffer))


(define-derived-mode edit-variable-mode
  lisp-interaction-mode
  "Edit-Variable"
  "Major mode for editing the contents of emacs variables.
\\{edit-variable-mode-map}"
  (make-local-variable 'edit-variable-buffer)
  (make-local-variable 'edit-variable-symbol)
  (set (make-local-variable 'revert-buffer-function)
       'edit-variable-revert-buffer))


(define-key edit-variable-mode-map
  (kbd "C-x C-s") 'edit-variable-save-buffer)
(define-key edit-variable-mode-map
  (kbd "C-c C-c") 'my-eval-and-kill-buffer)


;; (defun edit-variable-print-1 (value)
;;   (case (type-of value)
;;     (cons)
;;     (t (prin1 value))))

;; (defun edit-variable-print (value)
;;   (let ((print-level nil)
;;         (print-length nil)
;;         (print-circle t)
;;         (standard-output (current-buffer))
;;         (print-continuous-numbering t)
;;         (print-number-table nil))
;;     (edit-variable-print-1)))

;; (defun edit-variable-sanitize (value)
;;   (case (type-of value)
;;     ((integer symbol string float)
;;      value)
;;     (cons
;;      (condition-case nil
;;          (mapcar 'edit-variable-sanitize value)
;;        (wrong-type-argument
;;         (cons (edit-variable-sanitize (car value))
;;               (edit-variable-sanitize (cdr value))))))
;;     (vector
;;      (apply 'vector
;;             (mapcar* 'edit-variable-sanitize value)))
;;     (t
;;      'XXX)))

(defun edit-variable-revert-buffer (ignore-auto noconfirm)
  (erase-buffer)
  (insert (substitute-command-keys
           (concat
            ";; Use \\[edit-variable-save-buffer] to load the new definition and continue;\n"
            ";; Use \\[my-eval-and-kill-buffer] to load the new definition and quit.\n\n")))
  (let ((name (symbol-name edit-variable-symbol)))
    (if (and edit-variable-symbol (boundp edit-variable-symbol))
        (let ((value (and edit-variable-symbol (symbol-value edit-variable-symbol)))
              (is-buffer-local (or (local-variable-p edit-variable-symbol edit-variable-buffer)
                                   (local-variable-if-set-p edit-variable-symbol))))
          (when is-buffer-local
            (insert "(with-current-buffer edit-variable-buffer\n"))
          (insert "(setq\n" name "\n")
          (when is-buffer-local
            (insert ";; Buffer-local value in " (buffer-name edit-variable-buffer) "\n"))
          (let ((print-level nil)
                (print-length nil)
                (print-circle t)
                (standard-output (current-buffer))
                (value-start (point)))
            ;; Print the value of the variable into the buffer such
            ;; that it can hopefully be evaluated to produce a new
            ;; value that is equal to the old value.
            (insert "'")
            ;;(prin1 (edit-variable-sanitize value))
            (prin1 value)

            ;; Check that the printed expression can be read back to
            ;; produce the original value.  If it can't, replace the
            ;; expression with something that very obviously can't be
            ;; read.
            (unless (condition-case nil
                        (equal value
                               (eval
                                (read
                                 (buffer-substring value-start (point)))))
                      (error nil))
              (delete-region value-start (point))
              (insert "'#<unprintable value>")))
          (insert ")")
          (when is-buffer-local
            (insert ")"))
          (backward-sexp)
          (indent-pp-sexp t)
          (down-list (if is-buffer-local 2 1))
          (forward-sexp 3)
          (backward-sexp)
          (forward-char))
      (insert "(defconst " name "\n")
      (insert ";; NOTE: " name " is already bound as a function.\n")
      (insert "'nil\n)")
      (backward-sexp)
      (indent-pp-sexp t)
      (down-list)
      (forward-sexp 3)
      (backward-sexp)
      (forward-char)))
  (set-buffer-modified-p nil))



(defun edit-variable (name)
  "Interactively edit the value of a variable."
  (interactive
   (let ((var (thing-at-point 'symbol)))
     (list (completing-read (concat "Variable to edit"
                                    (when var
                                      (format " (default %s)" var))
                                    ": ")
                            obarray
                            'boundp
                            nil
                            nil
                            'edit-variable-history-list
                            var))))
  (let* ((orig-buffer (current-buffer))
         (symbol (intern-soft name)))
    (switch-to-buffer "*edit-variable*")
    (edit-variable-mode)
    (setq edit-variable-symbol symbol)
    (setq edit-variable-buffer orig-buffer)
    (revert-buffer)))

(provide 'edit-variable)
