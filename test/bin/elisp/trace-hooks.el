(require 's)

(defun find-hook-vars ()
  (let (hook-vars)
    (mapatoms (lambda (atom)
                (let ((name (symbol-name atom)))
                  (when (s-ends-with? "-hook" name)
                    (push atom hook-vars)))))
    hook-vars))

(provide 'trace-hooks)
