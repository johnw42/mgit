;; -*- no-byte-compile: t -*-
(global-set-key (kbd "C-c >") 'my-indent-region)
(global-set-key (kbd "C-c <") 'my-unindent-region)

(defvar my-indent-empty-lines nil)

;; (defmacro with-repeat-on-final-keystroke (&rest body)
;;   (declare (indent 0))
;;   ;; Code adapted from repeat.el
;;   (with-gensyms (repeat-char evt repeating)
;;     `(let ((,repeat-char last-command-event)
;;            repeating)
;;        (unwind-protect
;;            (while
;;                (let* ((,evt (read-key)))
;;                  ;; For clicks, we need to strip the meta-data to
;;                  ;; check the underlying event name.
;;                  (and (eq (or (car-safe ,evt) ,evt)
;;                           (or (car-safe ,repeat-char)
;;                               ,repeat-char))
;;                       (progn ,@body
;;                              t))))
;;          (push last-input-event unread-command-events)
;;          nil))))

(defun my-indent-region (beginning end count)
  (interactive "r\np")
  (let ((end-marker (copy-marker end)))
    (let ((deactivate-mark nil)
          repeated)
      (save-excursion
        (goto-char beginning)
        (beginning-of-line)
        (while (and (< (point)
                       (marker-position end-marker))
                    (prog1 t
                      (cond
                       ((> count 0)
                        (when (or my-indent-empty-lines
                                  (not (eolp)))
                          (dotimes (_ count)
                            (insert " "))))
                       ((< count 0)
                        (dotimes (_ (- count))
                          (when (looking-at-p " ")
                            (delete-char 1))))))
                    (forward-line 1)))))))

(defun my-unindent-region (beginning end count)
  (interactive "r\np")
  (my-indent-region beginning end (- count)))

(provide 'my-indent)

;; Local Variables:
;; no-byte-compile: t
;; End:
