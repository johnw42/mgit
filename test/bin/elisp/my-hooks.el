;; -*- no-byte-compile: t -*-
;; byte-compile-warnings: (not cl-functions);
(require 'cl)
(require 'if-match)
(require 'string-util)

(defvar my-hooks-file-hooks nil)

(defvar run-after-save nil)
(make-variable-buffer-local 'run-after-save)
(put 'run-after-save 'safe-local-variable
     (lambda (value)
       (or (booleanp value)
           (stringp value))))

(defvar local-hooks nil)
(make-variable-buffer-local 'local-hooks)
(put 'local-hooks 'safe-local-variable
     (lambda (value)
       (or (functionp value)
           (and (listp value)
                (every 'functionp value)))))

(add-hook 'find-file-not-found-functions 'my-hooks-find-file-not-found-function)
(add-hook 'after-save-hook 'my-hooks-after-save-hook)
(add-hook 'before-save-hook 'my-hooks-before-save-hook)
(add-hook 'find-file-hook 'my-hooks-find-file-hook)

(defun my-hooks-find-file-not-found-function ()
  (some 'funcall
        (my-hooks-runnable-funcs 'find-file-not-found-functions)))

(defun my-hooks-after-save-hook ()
  (when run-after-save
    (let ((process-environment (cons (concat "THIS=" (buffer-file-name))
                                     process-environment)))
      (compile (if (stringp run-after-save)
                   run-after-save
                 (buffer-file-name)))))
  (my-run-local-hooks 'after-save-hook))

(defun my-hooks-before-save-hook ()
  (my-run-local-hooks 'before-save-hook))

(defun my-hooks-find-file-hook ()
  (my-run-local-hooks 'find-file-hook))

(defun my-run-local-hooks (hook-var)
  (dolist (local-hook local-hooks)
    (funcall local-hook hook-var))
  (mapc 'funcall (my-hooks-runnable-funcs hook-var)))

(defun my-hooks-runnable-funcs (desired-hook-var)
  (let (result)
    (dolist (hook my-hooks-file-hooks)
      (let ((hook-func (elt hook 0))
            (hook-var (elt hook 1))
            (hook-mode (elt hook 2))
            (hook-path (elt hook 3))
            (hook-pattern (elt hook 4)))
        (when (and (eq hook-var desired-hook-var)
                   (or (not hook-mode) (eq hook-mode major-mode))
                   (or (not hook-path) (equal hook-path buffer-file-name))
                   (or (not hook-pattern) (let ((case-fold-search nil))
                                            (string-match-p hook-pattern buffer-file-name))))
          (push hook-func result))))
    (nreverse result)))

(defun add-major-mode-hook (mode hook)
  "Adds a hook function HOOK for the major mode MODE, and runs
HOOK in each open buffer whose major mode is already MODE."
  (let ((hook-var (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook-var hook)
    (when (fboundp hook)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (eq major-mode mode)
            (funcall hook)))))))

(defun* add-file-hook (hook-var hook-func &key mode path path-pattern append)
  "Adds a hook to HOOK-VAR that calls HOOK-FUNC when the current
buffer's filename or mode matches the given criteria.  Valid
choices for HOOK-VAR are

 * `find-file-not-found-functions'
 * `after-save-hook'
 * `before-save-hook'
 * `find-file-hook'"
  (assert (symbolp hook-var))
  (assert (symbolp hook-func))
  (assert (or (not mode) (symbolp mode)))
  (assert (or (not path) (stringp path)))
  (assert (or (not path-pattern) (stringp path-pattern)))
  (let ((new-elem (vector hook-func hook-var mode path path-pattern)))
    (add-to-list 'my-hooks-file-hooks new-elem append)))

(defun* remove-file-hook (hook-var hook-func &key mode path path-pattern append)
  (assert (symbolp hook-var))
  (assert (symbolp hook-func))
  (assert (or (not mode) (symbolp mode)))
  (assert (or (not path) (stringp path)))
  (assert (or (not path-pattern) (stringp path-pattern)))
  (setq my-hooks-file-hooks
        (remove (vector hook-func hook-var mode path path-pattern)
                my-hooks-file-hooks)))

(defadvice eval-defun (after my-hooks activate)
  "Automatically re-run major-mode hooks when they are redefined
interactively."
  (when (and (symbolp ad-return-value)
             (fboundp ad-return-value))
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (let* ((hooks-var (intern (format "%S-hook" major-mode)))
                 (major-mode-hooks (and (boundp hooks-var)
                                        (symbol-value hooks-var))))
            (when (memq ad-return-value major-mode-hooks)
              (incf count)
              (funcall ad-return-value)))))
      (unless (zerop count)
        (message "Ran major mode hook in %s buffers." count)))))

(defvar trace-hooks-buffer nil)
(defvar hook-counts (make-hash-table :test 'eq))

(define-minor-mode trace-hooks-mode
  "Minor mode to show when hooks are run."
  :init-value nil
  (if trace-hooks-mode
      (progn
        (setq trace-hooks-buffer (get-buffer-create "*Hooks*"))
        (pop-to-buffer trace-hooks-buffer)
        ;;(ad-enable-regexp "my-hooks-trace")
        )
    (clrhash hook-counts)
    (ad-disable-regexp "my-hooks-trace"))
  (ad-activate-regexp "my-hooks-trace")
  (do-symbols (hook)
    (when (and (boundp hook)
               (listp (symbol-value hook))
               (not (memq hook bad-hooks)))
      (let ((name (symbol-name hook)))
        (when-match ("^.*-\\(hook\\)$" name)
          (if-match (".*-mode-hook" name)
              nil
            (let ((trace-func (intern (concat (symbol-name hook)
                                              "-tracer-func"))))
              (if trace-hooks-mode
                  (progn
                    (eval `(defun ,trace-func (&rest _)
                             (record-hook-call ',hook "TRACE")))
                    (add-hook hook trace-func))
                (remove-hook hook trace-func)))))))))

(defconst bad-hooks '(post-gc-hook pre-command-hook post-command-hook echo-area-clear-hook))

(defun record-hook-call (&optional hook caller)
  (condition-case err
      (when trace-hooks-mode
        (unless (memq hook bad-hooks)
          (run-with-timer
           0 nil
           (lambda (now caller hook)
             (let ((count (incf (gethash hook hook-counts 0))))
               (unless (> count 100)
                 (with-current-buffer trace-hooks-buffer
                   (save-excursion
                     (goto-char (point-max))
                     (insert (format "%s %s: %S (%s)\n" now caller hook count)))))))
           (current-time-string) caller hook)))
    (error (message "error in record-hook-call: %S" err))))

(defadvice run-hooks (before my-hooks-trace)
  (dolist (hook (ad-get-args 0))
    (record-hook-call hook "run-hooks")))

(defadvice run-hook-with-args (before my-hooks-trace)
  (record-hook-call (ad-get-arg 0) "run-hook-with-args"))

(defadvice run-hook-with-args-until-success (before my-hooks-trace)
  (record-hook-call (ad-get-args 0) "run-hook-with-args-until-success"))

(defadvice run-hook-with-args-until-failure (before my-hooks-trace)
  (record-hook-call (ad-get-args 0) "run-hook-with-args-until-failure"))

(defun my-cronjob-hook (hook-var)
  (case hook-var
    (after-save-hook
     (call-process (buffer-file-name) nil nil nil "update"))))

(defun* my-call-stack (&optional start-after)
  (let ((frame-number 0)
        (found-self (not start-after))
        result
        frame-info)
    (while (setq frame-info (backtrace-frame frame-number))
      (let ((function-name (cadr frame-info)))
        (if found-self
            (push frame-info result)
          (if (eq function-name start-after)
              (setq found-self t))))
      (incf frame-number))
    (nreverse result)))

(defun my-trace-hook (&optional debug-me)
  "Prints a message saying which hook a function was called from."
  ;; (let ((frame-number 0)
  ;;       (prev-func-name t)
  ;;       frame-info
  ;;       hook-caller-name
  ;;       hook-func-name
  ;;       tentative-hook-caller-name)
  ;;   (while (and (or (not hook-func-name)
  ;;                   (not hook-caller-name))
  ;;               (setq frame-info (backtrace-frame frame-number)))
  ;;     (when (car frame-info)
  ;;       (let ((func-name (cadr frame-info)))
  ;;         (when (symbolp func-name)
  ;;           (cond
  ;;            ((eq func-name 'run-hooks)
  ;;             (setq hook-caller-name (caddr frame-info))
  ;;             (setq hook-func-name prev-func-name))
  ;;            ((eq func-name 'run-hook-with-args-until-success)
  ;;             (setq hook-caller-name nil)
  ;;             (setq hook-func-name (caddr frame-info)))
  ;;            ((eq prev-func-name 'run-hook-with-args-until-success)
  ;;             (setq hook-caller-name func-name))
  ;;            ((eq prev-func-name 'my-trace-hook)
  ;;             (setq hook-func-name func-name))
  ;;            ((eq prev-func-name hook-func-name)
  ;;             (setq tentative-hook-caller-name func-name)))
  ;;           (setq prev-func-name func-name))))
  ;;     (incf frame-number))
  ;;   (message "Running %S (via %S) in %s"
  ;;            hook-func-name
  ;;            (or hook-caller-name tentative-hook-caller-name)
  ;;            (buffer-name))
  ;;   (when debug-me
  ;;     (message "stack: %S" (my-call-stack 'my-call-stack))))
  ;; nil
  )

;; (defun my-after-change-function (new-start new-end old-legnth)
;;   (when (buffer-file-name)
;;     (my-trace-hook)))

;; (defun my-test-hook-func ()
;;   (my-trace-hook))

;; (defvar my-test-hook-var nil)

;; (add-hook 'my-test-hook-var 'my-test-hook-func)
;; (add-hook 'after-change-functions 'my-after-change-function)
(remove-hook 'after-change-functions 'my-after-change-function)

(provide 'my-hooks)

;; Local Variables:
;; no-byte-compile: t
;; End:
