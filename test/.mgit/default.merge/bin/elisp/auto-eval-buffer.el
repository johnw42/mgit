;; -*- no-byte-compile: t -*-
;; When saving files that have previouly been loaded, automatically
;; re-load them.

(require 'unit-test-runner)
(require 'unit-test-utils)
(require 'my-load-path)
(require 'string-util)
(eval-when-compile
  (require 'cl))

(add-hook 'after-save-hook 'auto-eval-buffer-maybe)

(defvar auto-eval-p nil
  "The value of this variable is t during automatic buffer
evaluation and nil otherwise.")

(defvar auto-eval-buffer-hook nil
  "Hooks run after a buffer is auto-evaluated.")

(defvar auto-eval-buffer-debug-on-error nil
  "*If true, set `debug-on-error' to t when automatically
evaluating a buffer.")

(defvar before-auto-eval-hook nil
  "Hooks run before auto-evaluating a buffer.")

(defvar after-auto-eval-hook nil
  "Hooks run after auto-evaluating a buffer, but only if the
evaluation is successful.")

(defvar auto-eval-derived-modes t
  "Either t or a list of major modes affected the buffer being
auto-evaluated.")

(define-minor-mode global-auto-eval-mode
  "Minor mode to automatically evaluate Lisp buffers when they are saved."
  nil nil nil
  :global t)

(defun auto-eval-buffer-maybe ()
  "Evaluates a buffer if it is eligible for automatic evaluation."
  (when (and global-auto-eval-mode
             (not auto-eval-p))
    (let ((auto-eval-p t))
      (when (should-auto-eval-buffer)
        (if (zerop (recursion-depth))
            (do-auto-eval-buffer)
          (message "Skipping auto-evaluating because of recursive edit."))))))

(defun should-auto-eval-buffer ()
  "Tests whether the current buffer should be evaluated
automatically when it is saved."
  (and (buffer-file-name)
       (string-match-p "\.el$" (buffer-file-name))
       (derived-mode-p 'emacs-lisp-mode)
       (or (in-load-history-p (buffer-file-name))
           (in-load-history-p (unit-test-tested-file buffer-file-name)))
       t))

(defadvice define-derived-mode (after auto-eval activate)
  "Record when a buffer defines a new mode.  This information is
used to re-apply the mode defined by this buffer in any existing
buffers that use the new mode."
  (unless (eq t auto-eval-derived-modes)
    (push (ad-get-arg 0) auto-eval-derived-modes)))

(defadvice add-hook (after auto-eval activate)
  "Modify add-hook so that when a mode-related hook is added
during auto-evaluation, the hook is immediately executed in every
buffer where that mode is active."
  (unless (eq t auto-eval-derived-modes)
    (save-match-data
      (let* ((hook-var (ad-get-arg 0))
             (hook-name (and (symbolp hook-var)
                             (symbol-name hook-var)))
             (case-fold-search nil)
             (mode-name (and hook-name
                             (string-match "^\\(.*-mode\\)-hook$" hook-name)
                             (match-string 1 hook-name)))
             (mode-var (intern-soft mode-name)))
        (when mode-var
          (push mode-var auto-eval-derived-modes))))))

(defun do-auto-eval-buffer ()
  "Evluates the current buffer with special logic for automatic
evluation.

For buffers that have previously been compiled, the buffer is
re-compiled and the compiled code is executed."
  (setq auto-eval-derived-modes nil)
  (let ((file-to-load buffer-file-name)
        (compiled-name (byte-compile-dest-file buffer-file-name))
        (debug-on-error (or debug-on-error
                            auto-eval-buffer-debug-on-error))
        (warning-series t)
        succeeded
        auto-eval-derived-modes)
    ;; Handle compiled files specially.  This is important for
    ;; correctness in some cases.
    (when (file-exists-p compiled-name)
      (if (byte-compile-file buffer-file-name)
          (setq file-to-load compiled-name)
        (message "Error compiling buffer.")))
    (when file-to-load
      ;; (condition-case err
      ;;     (update-file-autoloads buffer-file-name)
      ;;   (error
      ;;    (message "Error generating autoloads: %S" err)))
      (run-hooks 'before-auto-eval-hook)
      (condition-case err
          (let ((start-message "Evaluating buffer contents..."))
            (message start-message)
            ;; Actual evaluation happens here.
            (load-file file-to-load)
            (setq succeeded t)
            (when (equal (current-message) start-message)
              (message (concat start-message "done."))))
        ((debug error)
         (message "Error evaluating buffer: %S" err))))
    ;; If the buffer defines a major mode, find every buffer where the
    ;; mode is already active and re-apply the mode.  Unmodified
    ;; buffers are reverted before re-applying the mode.
    (when auto-eval-derived-modes
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (apply 'derived-mode-p auto-eval-derived-modes)
            (let ((old-major-mode major-mode))
              (unless (buffer-modified-p)
                (ignore-errors
                  (revert-buffer nil nil t)
                  (message "Reverted %s" (buffer-name))))
              (funcall old-major-mode)
              (message "Re-applied %S in %s" old-major-mode (buffer-name)))))))
    (when succeeded
      (message "Auto-evaluated buffer.")
      (run-hooks 'after-auto-eval-hook))))

(provide 'auto-eval-buffer)

;; Local Variables:
;; no-byte-compile: t
;; End:
