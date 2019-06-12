;; -*- no-byte-compile: t -*-
(eval-when-compile
  (require 'cl))

(defvar my-shift-region-basic-offset-alist
  '((python-mode . python-indent)
    (c-mode . c-basic-offset)
    (js2-mode . js2-basic-offset)
    (js3-mode . js3-indent-level)))

(defun prefix-negative-p (raw-prefix)
  (< (prefix-numeric-value raw-prefix) 0))

(defun negate-prefix (raw-prefix)
  "Returns a value that satisfies the following invariants:

  (= (prefix-numeric-value (negate-prefix RAW-PREFIX)
     (- (prefix-numeric-value RAW-PREFIX))))

  (eq (symbolp RAW-PREFIX)
      (symbolp (negate-prefix RAW-PREFIX)))

  (eq (integerp RAW-PREFIX)
      (integerp (negate-prefix RAW-PREFIX)))

  (eq (consp RAW-PREFIX)
      (consp (negate-prefix RAW-PREFIX)))"
  (cond
   ((null raw-prefix) '-)
   ((eq '- raw-prefix) nil)
   ((integerp raw-prefix) (- raw-prefix))
   ((consp raw-prefix) (list (- (car raw-prefix))))))

(defun my-shift-region-prefix-numeric-value (prefix)
  (case prefix
    ((nil) (or
            (dolist (pair my-shift-region-basic-offset-alist)
              (let ((mode (car pair))
                    (symbol (cdr pair)))
                (when (derived-mode-p mode)
                  (return (symbol-value symbol)))))
            1))
    (- (- (my-shift-region-prefix-numeric-value nil)))
    (t (prefix-numeric-value prefix))))

(defun shift-region-right (count)
  "Insert COUNT spaces at the start of each line that overlaps the region,
and adjust the region to exactly contain the modified lines."
  (interactive "P")
  (if (prefix-negative-p count)
      (shift-region-left (negate-prefix count))
    (setq count (my-shift-region-prefix-numeric-value count))
    (let ((end-marker (copy-marker (region-end) t))
          (to-insert (make-string count ? ))
          new-point)
      (goto-char (region-beginning))
      (beginning-of-line)
      (setq new-point (point))
      (while (< (point) (marker-position end-marker))
        (unless (looking-at " *$")
          (insert to-insert))
        (forward-line))
      (set-mark (point))
      (goto-char new-point))))

(defun shift-region-left (count)
  "Delete COUNT spaces at the start of each line that overlaps the region,
and adjust the region to exactly contain the modified lines.  Does nothing if
if any line starts with fewer than COUNT spaces."
  (interactive "P")
  (if (prefix-negative-p count)
      (shift-region-right (negate-prefix count))
    (setq count (my-shift-region-prefix-numeric-value count))
    (let ((end-marker (copy-marker (region-end) t))
          (pattern (make-string count ? ))
          err
          delete-positions
          new-point)
      (goto-char (region-beginning))
      (beginning-of-line)
      (setq new-point (point))
      (while (< (point) (marker-position end-marker))
        (when (looking-at " *[^ \n]")
          (if (< count (- (match-end 0) (match-beginning 0)))
              (push (point) delete-positions)
            (setq err t)))
        (forward-line))
      (if err
          (message "Can't shift lines left.")
        (dolist (pos delete-positions)
          (goto-char pos)
          (delete-char count)))
      (goto-char (1- (marker-position end-marker)))
      (end-of-line)
      (set-mark (point))
      (goto-char new-point))))

(provide 'my-shift-region)

;; Local Variables:
;; no-byte-compile: t
;; End:
