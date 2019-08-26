;; -*- no-byte-compile: t -*-
(defvar my-background-update-queue nil)
(make-variable-buffer-local 'my-background-update-queue)

(defun schedule-background-update (func)
  (callf append my-background-update-queue (list func)))

(defun my-background-update-buffer-list-update-hook ()
  (condition-case nil
      (let ((orig-buffer (current-buffer)))
        (dolist (buffer (buffer-list))
          (unless (eq buffer orig-buffer)
            (with-current-buffer buffer
              (when my-background-update-queue)
              )
            )))
    (error nil)))

(add-hook 'buffer-list-update-hook
          'my-background-update-buffer-list-update-hook)

(provide 'my-background-update)

;; Local Variables:
;; no-byte-compile: t
;; End:
