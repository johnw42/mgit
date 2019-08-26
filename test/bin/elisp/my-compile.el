;; -*- no-byte-compile: t -*-
;;; Configure compile-mode to always search for files in the
;;; directories of open buffers.

(require 'compile)

(defun my-compile-compilation-start-hook (&rest args)
  (let ((new-path '(nil)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (stringp default-directory)
                   (not (member default-directory new-path)))
          (push default-directory new-path))))
    (setq compilation-search-path (nreverse new-path)))
  (let ((log-buffer (get-buffer "*compilation-hook-log*")))
    (when log-buffer
      (with-current-buffer log-buffer
        (delete-region (point-min) (point-max))))))

(add-hook 'compilation-start-hook
          'my-compile-compilation-start-hook)

;; (defun my-compile-compilation-trace (show-header &optional format-string &rest args)
;;   (let ((log-buffer (get-buffer-create "*compilation-hook-log*")))
;;     (with-current-buffer log-buffer
;;       (goto-char (point-max)))
;;     (when show-header
;;       (let ((point (point))
;;             (point-max (point-max))
;;             (text-before-point (buffer-substring-no-properties
;;                                 compilation-filter-start
;;                                 (point)))
;;             (text-after-point (buffer-substring-no-properties
;;                                (point)
;;                                (point-max))))
;;         (with-current-buffer log-buffer
;;           (insert
;;            (format "\n%s:%s:%s:\nbefore-point: %S\nafter-point:%S\n"
;;                    compilation-filter-start
;;                    point
;;                    point-max
;;                    text-before-point
;;                    text-after-point)))))
;;     (when format-string
;;       (with-current-buffer log-buffer
;;         (insert (apply 'format format-string args) "\n")))))

(defvar my-compilation-partial-line nil)

(defun my-compilation-filter-hook ()
  ;; (my-compile-compilation-trace t)

  ;; Un-hide previously hidden partial line.
  (make-local-variable 'my-compilation-partial-line)
  (when my-compilation-partial-line
    (put-text-property (car my-compilation-partial-line)
                       (cdr my-compilation-partial-line) 'invisible nil))

  ;; Hide partial line.
  (goto-char (point-max))
  (beginning-of-line)
  (setq my-compilation-partial-line (cons (point) (point-max)))
  (put-text-property (car my-compilation-partial-line)
                     (cdr my-compilation-partial-line)
                     'invisible t)

  (save-restriction
    (narrow-to-region (save-excursion
                        (goto-char compilation-filter-start)
                        (forward-line -2)
                        (point))
                      (point-max))
    ;; (my-compile-compilation-trace nil "New restriction: %s..%s"
    ;;                               (point-min) (point-max))

    ;; Cast-specific hack:
    ;; Delete warnings about directory outputs.
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward
              (rx bol
                  "WARNING: "
                  (* nonl)
                  "/google3/"
                  (or "chrome_platform/cast"
                      "java/com/google/chrome/cloudview")
                  "/script/"
                  (* nonl)
                  " is a directory; dependency checking of directories is unsound.\n")
              nil t)
        (delete-region (match-beginning 0)
                       (match-end 0))
        ;; (my-compile-compilation-trace t "Deleted warning.")
        ))

    ;; Blaze-specific hack:
    ;; Collapse groups of lines starting with "____".
    (save-excursion
      (while (re-search-backward "^\\(____\\)\\([^\n]*\\)" nil t)
        (let (start end)
          (put-text-property (match-beginning 1)
                             (match-end 1)
                             'invisible t)
          (add-text-properties (match-beginning 2)
                               (match-end 2)
                               (list 'fontified t
                                     'font-lock-face 'font-lock-function-name-face))
          ;; (my-compile-compilation-trace
          ;;  t "Kept ____ line from %s to %s"
          ;;  (match-beginning 1) (match-end 2))
          ;; (save-excursion
          ;;   (goto-char (match-end 1))
          ;;   (insert "++++"))
          (setq end (point-marker))
          (while (and (zerop (forward-line -1))
                      (looking-at "____"))
            (setq start (point-marker)))
          (when (and start end)
            ;; (save-excursion
            ;;   (goto-char end)
            ;;   (insert ">>>")
            ;;   (goto-char start)
            ;;   (insert "<<<"))
            (put-text-property start end 'invisible t)
            ;; (my-compile-compilation-trace
            ;;  t "Hid ____ lines from %s to %s"
            ;;  start end)
            ))))
    ))

(add-hook 'compilation-filter-hook 'my-compilation-filter-hook t)

(provide 'my-compile)

;; Local Variables:
;; no-byte-compile: t
;; End:
