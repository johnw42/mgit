;; -*- no-byte-compile: t -*-
(eval-when-compile
  (require 'cl))

(defvar my-module-table)

(defun my-module-subst (form)
  (cond ((symbolp form)
         (or (gethash form my-module-table)
             (let* ((name (symbol-name form))
                    (new-form (if (string-match-p "^-\\|--" name)
                                  (gensym name)
                                form)))
               (puthash form new-form my-module-table)
               new-form)))
        ((or (consp form)
             (vectorp form))
         (map
          (if (consp form) 'list 'vector)
          (lambda (subform)
            (my-module-subst subform))
          form))
        (t form)))

(defmacro my-module (&rest body)
  (let ((my-module-table (make-hash-table :test 'eq)))
    (my-module-subst `(progn ,@body))))

(provide 'my-module)

;; Local Variables:
;; no-byte-compile: t
;; End:
