;; -*- no-byte-compile: t -*-
;; (defvar compile-on-save-command nil)
;; (make-variable-buffer-local 'compile-on-save-command)
;; (put 'compile-on-save-command 'safe-local-variable 'stringp)

;; (define-minor-mode compile-on-save-mode
;;   "Compile on save mode."
;;   :init-value nil
;;   :lighter COS
;;   )

(defun exec-on-save ()
  (message "exec-on-save"))

(provide 'my-compile-on-save)

;; Local Variables:
;; no-byte-compile: t
;; End:
