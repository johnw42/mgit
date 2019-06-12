;; -*- no-byte-compile: t -*-
(require 'cl)

;; (defvar unit-test-messages nil
;;   "Messages collected while running a unit test.")

;; (defvar unit-test-in-runner nil)

;; (defvar unit-test-num-passed 0)

;; (defvar unit-test-num-failed 0)

;; (defun unit-test-message (&rest args)
;;   (if unit-test-in-runner
;;       (car (push (apply 'format args) unit-test-messages))
;;     (let ((unit-test-running nil))
;;       (apply 'message args))))

;; (defconst unit-test-message-prefix (getenv "UNIT_TEST_MESSAGE_PREFIX"))

;; (defadvice message (before unit-test-runner activate)
;;   (when unit-test-message-prefix
;;     (ad-set-arg 0 (concat unit-test-message-prefix (ad-get-arg 0)))))

;; (defun unit-test-run (&optional test-file)
;;   (let ((test-file (or test-file (getenv "UNIT_TEST_NAME")))
;;         (unit-test-messages nil)
;;         (unit-test-num-passed 0)
;;         (unit-test-num-failed 0)
;;         (unit-test-runner-active t))
;;     (condition-case error
;;         (progn
;;           (with-temp-buffer
;;             (insert-file-contents test-file)
;;             (let ((unit-test-in-runner t))
;;               (eval-buffer)))
;;           (if unit-test-messages
;;               (dolist (result (reverse unit-test-messages))
;;                 (unit-test-message "%s: %s" test-file result))
;;             (unit-test-message "%s: no tests" test-file)))
;;       ((debug error)
;;        (incf unit-test-num-failed)
;;        (unit-test-message "%s: %S" test-file error)))))

(defun unit-test-run (test-file)
  (let* ((temp-buffer (get-buffer-create
                       (format "*Unit test results: %s*"
                               (file-name-nondirectory test-file)))))
    (pop-to-buffer temp-buffer)
    (with-current-buffer temp-buffer
      (delete-region (point-min) (point-max))
      (setq exit-code
            (call-process
             ;; program
             (concat invocation-directory invocation-name)
             ;; infile
             nil
             ;; destination: the current buffer
             t
             ;; display: display buffer Uas it is written
             t
             ;; command-line args
             "--batch"
             "--script"
             test-file
             "--eval"
             "(message \"foo\")"))
      (insert (format "\nExit code: %s\n" exit-code)))
    (when (zerop exit-code)
      ;;(kill-buffer temp-buffer)
      )))

(defun unit-test-after-auto-eval-hook ()
  (let ((test-file-name (unit-test-test-file buffer-file-name)))
    (when (file-readable-p test-file-name)
      (unit-test-run test-file-name))))

(add-hook 'after-auto-eval-hook 'unit-test-after-auto-eval-hook)

(provide 'unit-test-runner)

;; Local Variables:
;; no-byte-compile: t
;; End:
