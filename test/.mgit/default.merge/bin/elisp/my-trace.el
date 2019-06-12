;; -*- no-byte-compile: t -*-
;; -*- lexical-binding: t; -*-
(require 'my-interactive)
(require 'loadhist)
(require 'elp)

(defvar my-trace-function-history nil)

(defvar my-trace-tracing nil)
(defvar my-trace-traced-functions nil)
(defvar my-trace-buffer nil)
(defvar my-trace-timer (run-at-time 0
                                    1
                                    'my-trace-timer-func))

(defun my-trace-timer-func ()
  (when my-trace-buffer
    (with-current-buffer (get-buffer-create "*Trace*")
      (let ((was-at-end (eobp)))
        (goto-char (point-max))
        (let ((buffer my-trace-buffer)
              (my-trace-buffer nil))
          (dolist (item (reverse buffer))
            (insert (format "%S\n" item))))
        (setq my-trace-buffer nil)
        (when was-at-end
          (goto-char (point-max)))))))

(defun my-trace-function-before-1 (fn args)
  (with-current-buffer (get-buffer-create "*Trace*")
    (goto-char (point-max))
    (let ((frame-number 0)
          (ignore-calls (get fn 'my-trace-ignore-calls))
          found-self
          function-names
          ignore)
      (while
          (let ((frame-info (backtrace-frame frame-number)))
            (when frame-info
              (let ((function-name (cadr frame-info)))
                (unless found-self
                  (if (eq function-name fn)
                      (setq found-self t)))
                (when found-self
                  (when (memq function-name ignore-calls)
                    (setq ignore t))
                  (push function-name function-names)))
              t))
        (incf frame-number))
      (callf nreverse function-names)
      (when (and (not ignore)
                 (not (and (get fn 'my-trace-ignore-from-bytecode)
                           (eq 'byte-code (cadr function-names)))))
        (insert
         (propertize (format "%S" fn) 'face 'bold)
         (format " %S" (cdr function-names))
         "\n")))))

(defun my-trace-function-before (fn args)
  (when (not my-trace-tracing)
    (let ((my-trace-tracing t))
      (my-trace-function-before-1 fn args))))

(defun my-trace-function (fn &optional tag)
  (interactive (list (my-read-function
                      "Function to trace"
                      'nil
                      'my-trace-function-history)))
  (unless (get fn 'my-trace-function)
    (eval `(defadvice ,fn (before my-trace-function activate)
             ,(if tag
                  `(push ',tag my-trace-buffer)
                `(push (cons ',fn (ad-get-args 0))
                       my-trace-buffer))))
    (put fn 'my-trace-function t)))

(defun my-untrace-function (fn)
  (interactive (list (my-read-function
                      "Function to untrace"
                      (lambda (sym)
                        (get sym 'my-trace-function))
                      'my-trace-function-history)))
  (when (get fn 'my-trace-function)
    (ad-remove-advice fn 'before 'my-trace-function)
    (ad-activate fn)
    (put fn 'my-trace-function nil)))

(defun my-untrace-all ()
  (interactive)
  (mapatoms 'my-untrace-function))

(defun my-trace-hooks (local)
  (maphoooks
   (lambda (hook-sym fns)
     (dolist (fn fns)
       (my-trace-function fn hook-sym)))))

(defun my-instrument-hooks (&optional local)
  (maphooks
   (lambda (hook-sym fns)
     (dolist (fn fns)
       (elp-instrument-function fn)))))

(defun maphooks (fn &optional local)
  (let ((local-vars (and local (buffer-local-variables))))
    (mapatoms
     (lambda (hook-sym)
       (when (boundp hook-sym)
         (let ((name (symbol-name hook-sym)))
           (when (and (not (memq hook-sym '(byte-compile-cl-functions)))
                      (or (string-match-p ".*-hooks?$" name)
                          (memq hook-sym unload-feature-special-hooks))
                      (or (not local)
                          (memq hook-sym local-vars)))
             (let ((values (symbol-value hook-sym))
                   filtered-values)
               (when (symbolp values)
                 (setq values (list values)))
               (while (consp values)
                 (let ((value (pop values)))
                   (when (and (symbolp value)
                              (fboundp value)
                              (functionp (symbol-function value)))
                     (push value filtered-values))))
               (callf nreverse filtered-values)
               (funcall fn hook-sym filtered-values)))))))))

(put 'set-match-data 'my-trace-ignore-from-bytecode nil)

(put 'set-match-data 'my-trace-ignore-calls
     '())

(provide 'my-trace)

;; Local Variables:
;; no-byte-compile: t
;; End:
