;; -*- no-byte-compile: t -*-
(define-minor-mode closure-imports-mode
  "Global minor mode for automatically maintaining imports in
  Closure code."
  :global t
  :init-value nil

  (if closure-imports-mode
      (add-hook 'before-save-hook
                'closure-imports-mode-before-save-hook)
    (remove-hook 'before-save-hook
                 'closure-imports-mode-before-save-hook)))

(defun closure-imports-mode-before-save-hook ()
  )

(provide 'closure-imports-mode)

;; Local Variables:
;; no-byte-compile: t
;; End:
