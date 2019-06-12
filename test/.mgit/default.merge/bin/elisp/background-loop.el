;; -*- lexical-binding: t; -*-
(eval-when-compile (require 'cl))

;; (cl-defmacro background-loop (loop-id &rest args)
;;   `(progn
;;      (when ,loop-id
;;        (background-loop-stop loop-id)
;;        (setf loop-id nil))
;;      (background-loop-start ,@args)))

(defun* background-loop-start (body-func
                               &key
                               (test nil)
                               (duty-cycle 0.01)
                               (min-interval 1))
  "Call BODY-FUNC, and keep calling it prediodically in the
background for as long as TEST-FUNC returns non-nil.  Successive
calls are at least MIN-INTERVAL seconds apart, and the timer
interval is automatically adjusted so that BODY-FUNC is not
running most of the time.  The probability that BODY-FUNC will be
running at any given moment is set by DUTY-CYCLE.

Errors in BODY-FUNC are ignored, but errors in TEST-FUNC cause
the loop to stop.

The return value is a loop ID that can be passed to
`background-loop-stop' to abort this loop."

  (assert (< 0 duty-cycle))
  (assert (< duty-cycle 1))
  (assert (< 0 min-interval))

  (let (loop-body stopped)
    (setq loop-body
          (lambda ()
            (when (and (not stopped)
                       (or (not test)
                           (funcall test)))
              (let (start-time end-time timer-interval)
                ;; Do what the caller wanted us to do, recording the
                ;; start and end times.
                (setq start-time (float-time))
                (with-demoted-errors
                  (funcall body-func))
                (setq end-time (float-time))

                ;; Compute the delay before the next iteration of the loop.
                (setq timer-interval (max min-interval
                                          (/ (- end-time start-time)
                                             duty-cycle)))

                ;; Schedule the next iteration of the loop.
                (run-at-time timer-interval nil loop-body)))))

    ;; Start the loop running.
    (funcall loop-body)

    ;; Return a "loop ID", which is actually function that stops the
    ;; loop.
    (lambda ()
      (setq stopped t))))

(defun background-loop-stop (loop-id)
  "Stop a loop previously started by `background-loop-start'.  It
is safe to cal this function when LOOP-ID is nil, and to call it
multiple times for the same value of LOOP-ID."
  (when loop-id
    (funcall loop-id))
  nil)

(provide 'background-loop)
