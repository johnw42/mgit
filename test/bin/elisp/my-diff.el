;; -*- no-byte-compile: t -*-
;; -*- lexical-binding: t; -*-

(defun my-diff-to-old (start end)
  "Converts a region in unified diff format to contain just the
content from the old file."
  (interactive "r")
  (my-diff-convert-region start end 'old))

(defun my-diff-to-new (start end)
  "Converts a region in unified diff format to contain just the
content from the new file."
  (interactive "r")
  (my-diff-convert-region start end 'new))

(defun my-diff-convert-region (start end to-keep)
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (not (eobp))
      (cond
       ((looking-at (case to-keep (old "^[- ]") (new "^[+ ]")))
        (delete-char 1)
        (forward-line))
       ((looking-at (case to-keep (old "^\\+") (new "^-")))
        (let ((line-start (point)))
          (forward-line)
          (delete-region line-start (point))))
       (t (error "Premature end of diff region."))))))

(provide 'my-diff)

;; Local Variables:
;; no-byte-compile: t
;; End:
