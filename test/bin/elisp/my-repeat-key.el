(eval-when-compile (require 'cl))

(defvar key-repetition-count nil
  "The number of times the current command has been invoked by
repeating the kind key of its key sequence.  When a command is
invoked using the full key sequence, this variable will be set 0.
When it current command has not been invoked through a key
sequence with a repeatable final key, the value of this varialbe
is nil.")

;; (defun define-repeatable-key (keymap key command)
;;   "Defines a key sequence in that may be repeated by re-typing
;; the last character of the sequence.  The value of
;; `key-repetition-count' is incremented by 1 each time the command
;; is run by repeating the final keystroke."
;;   (define-key keymap key (repeatable-key-binding command)))

;; (defun repeatable-key-binding (command)
;;   (let ((new-command (intern
;;                       (concat "-"
;;                               (symbol-name command)
;;                               "--with-key-repetition"))))
;;     (eval `(defun ,new-command ()
;;              ,(concat
;;                "Runs `" (symbol-name command) "' with key repetition.\n\n"
;;                "Documentation for " (symbol-name command) ":\n\n"
;;                (documentation command))
;;              (interactive)
;;              (call-with-key-repetition ',command)))
;;     new-command))

;; (defun global-set-repeatable-key (key command)
;;   "Like `global-set-key', but calls `define-repeatable-key'
;; instead of `define-key'."
;;   (global-set-key key (repeatable-key-binding command)))

;; (defun local-set-repeatable-key (key command)
;;   "Like `local-set-key', but calls `define-repeatable-key'
;; instead of `define-key'."
;;   (local-set-key key (repeatable-key-binding command)))

;; (defun call-with-key-repetition (command)
;;   ;; Code adapted from repeat.el
;;   (let ((repeat-char last-command-event)
;;         (key-repetition-count 0)
;;         command-args)
;;     (call-interactively command t)
;;     (setq command-args (cdar command-history))
;;     (while
;;         (let* ((evt (read-key)))
;;           ;; For clicks, we need to strip the meta-data to
;;           ;; check the underlying event name.
;;           (if (eq (or (car-safe evt) evt)
;;                   (or (car-safe repeat-char)
;;                       repeat-char))
;;               (progn (incf key-repetition-count)
;;                      (undo-boundary)
;;                      (apply command command-args)

;;                      t)
;;             (push last-input-event unread-command-events)
;;             nil)))))

(defun make-repeatable (command)
  "Advises function COMMAND such that, when it is invoked
interactively by a multi-key sequence, typing the last key of the
sequence will execute the command until some other key is
pressed."
  (ad-add-advice
   command
   '(make-repeatable
     t t
     (advice lambda ()
             (if (not (called-interactively-p 'any))
                 ad-do-it
               (let ((repeat-char last-command-event)
                     (key-repetition-count 0))
                 ad-do-it
                 (while
                     (let* ((evt (read-key)))
                       ;; For clicks, we need to strip the meta-data to
                       ;; check the underlying event name.
                       (if (eq (or (car-safe evt) evt)
                               (or (car-safe repeat-char)
                                   repeat-char))
                           (progn (incf key-repetition-count)
                                  (undo-boundary)
                                  ad-do-it
                                  t)
                         (push last-input-event unread-command-events)
                         nil)))))))
   'around
   'last)
  (ad-activate command))

;; (progn
;;   (global-set-key (kbd "C-x o") 'other-window)
;;   (ad-unadvise 'other-window)
;;   (make-repeatable-with-advice 'other-window)
;;   (ad-activate 'other-window))

;; (progn
;;   ;; (ad-remove-advice 'other-window 'around 'make-repeatable)
;;   ;; (ad-update 'other-window)
;;   (ad-unadvise 'other-window)
;;   (global-set-repeatable-key (kbd "C-x o") 'other-window)
;;   )


;; (defun -make-repeatable (command)
;;   "Advises function COMMAND such that, when it is invoked
;; interactively by a multi-key sequence, typing the last key of the
;; sequence will execute the command until some other key is
;; pressed."
;;   (ad-add-advice
;;    command
;;    '(make-repeatable
;;      t t
;;      (advice lambda ()
;;              (if (not (called-interactively-p 'any))
;;                  ;; For non-interactive calls, just run the command normally.
;;                  ad-do-it
;;                (let* ((repeat-key-vector (this-command-keys))
;;                       (repeat-char last-command-event)
;;                       (key-repetition-count 0))
;;                  (assert (eq (key-binding repeat-key-vector)
;;                              commad))
;;                  ad-do-it
;;                  (while
;;                      (let* ((evt (read-key)))
;;                        ;; For clicks, we need to strip the meta-data to
;;                        ;; check the underlying event name.
;;                        (if (eq (or (car-safe evt) evt)
;;                                (or (car-safe repeat-char)
;;                                    repeat-char))
;;                            (progn (incf key-repetition-count)
;;                                   (undo-boundary)
;;                                   (message "XXX2 %S" repeat-char)
;;                                   ad-do-it
;;                                   t)
;;                          (push last-input-event unread-command-events)
;;                          nil)))))))
;;    'around
;;    'last)
;;   (ad-activate command))

;; (progn
;;   (defun -make-repeatable (command)
;;     "Advises function COMMAND such that, when it is invoked
;; interactively by a multi-key sequence, typing the last key of the
;; sequence will execute the command until some other key is
;; pressed."
;;     (ad-add-advice
;;      command
;;      '(make-repeatable
;;        t t
;;        (advice lambda ()
;;                (if (not (called-interactively-p 'any))
;;                    ad-do-it
;;                  (let ((repeat-char last-command-event)
;;                        (key-repetition-count 0))
;;                    ad-do-it
;;                    (while
;;                        (let* ((evt (read-key)))
;;                          ;; For clicks, we need to strip the meta-data to
;;                          ;; check the underlying event name.
;;                          (if (eq (or (car-safe evt) evt)
;;                                  (or (car-safe repeat-char)
;;                                      repeat-char))
;;                              (progn (incf key-repetition-count)
;;                                     (undo-boundary)
;;                                     ad-do-it
;;                                     t)
;;                            (push last-input-event unread-command-events)
;;                            nil)))))))
;;      'around
;;      'last)
;;     (ad-activate command))

;; (-make-repeatable 'shift-region-right)
;; (-make-repeatable 'shift-region-left)
;;)


(provide 'my-repeat-key)
