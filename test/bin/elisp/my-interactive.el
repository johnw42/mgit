;; -*- no-byte-compile: t -*-
;; -*- lexical-binding: t; -*-
(require 'find-func)

(defun my-read-symbol (prompt predicate default-value history)
  "Interactively read a symbol name."
  (unless prompt
    (setq prompt "Symbol"))
  (let ((enable-recursive-minibuffers t)
        val)
    (setq val (completing-read (if default-value
                                   (format "%s (default %s): " prompt default-value)
                                 (concat prompt ": "))
                               obarray predicate t nil nil
                               (and default-value (symbol-name default-value))))
    (if (equal val "")
        sym (intern val))))

(defun my-read-function (prompt predicate history)
  "Interactively read a function name."
  (my-read-symbol (or prompt "Function")
                  (lambda (x)
                    (and (fboundp x)
                         (or (not predicate)
                             (funcall predicate x))))
                  (function-called-at-point)
                  history))

(defun my-read-elisp-function (prompt predicate history)
  "Interactively read a variable name."
  (my-read-symbol (or prompt "Function")
                  (lambda (x)
                    (and (fboundp x)
                         (or (not predicate)
                             (funcall predicate x))))
                  (function-called-at-point)
                  history))

(defun my-read-lisp-variable (prompt predicate history)
  "Interactively read a variable name."
  (my-read-symbol (or prompt "Variable")
                  (lambda (x)
                    (and (boundp x)
                         (or (not predicate)
                             (funcall predicate x))))
                  (let ((var (variable-at-point)))
                    (and (symbolp var) var))
                  history))

(defun my-read-function-or-variable (prompt predicate history)
  "Interactively read a function name."
  (my-read-symbol (or prompt "Function or variable")
                  (lambda (x)
                    (and (or (boundp x)
                             (fboundp x))
                         (or (not predicate)
                             (funcall predicate x))))
                  (let ((var (variable-at-point t)))
                    (if (symbolp var)
                        var
                      (function-called-at-point)))
                  history))

(provide 'my-interactive)

;; Local Variables:
;; no-byte-compile: t
;; End:
