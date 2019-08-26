;; -*- no-byte-compile: t -*-
(defvar my-extended-command-p nil)

(defvar my-extended-commands '(xyzzy))

;; (defadvice commandp (around my-extended-command activate)
;;   (or ad-do-it
;;       (and my-extended-command-p
;;            (memq (ad-get-arg 0) my-extended-commands))))

;; (defadvice completion-try-completion (around my-extended-command activate)
;;   (if (not my-extended-command-p)
;;       ad-do-it
;;     (let (xyzzy)

;;       ad-do-it)))

(defadvice execute-extended-command (around my-extended-command activate)
  (let ((my-extended-command-p t))
    ;; (message "advised1")
    ad-do-it))

(provide 'my-extended-command)

;; Local Variables:
;; no-byte-compile: t
;; End:
