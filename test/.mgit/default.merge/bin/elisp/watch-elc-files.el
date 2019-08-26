;; -*- lexical-binding: t; -*-
(require 'background-loop)
(require 's)

(defvar watch-elc-files-loop-id nil)
(defvar watch-elc-files-file-times (make-hash-table :test 'equal))

;;;###autoload
(define-minor-mode watch-elc-files-mode
  "Minor mode to watch loaded elisp files for changes and reload them."
  :global t
  :lighter " W"

  (background-loop-stop watch-elc-files-loop-id)
  (when watch-elc-files-mode
    (setq watch-elc-files-loop-id
          (background-loop-start
           'watch-elc-files-scan-files
           :test (lambda () watch-elc-files-mode)))))

(defun watch-elc-files-scan-files ()
  "Function called periodically to check for files that need to be reloaded."
  ;;(message "watch-elc-files: scanning")
  (dolist (entry load-history)
    (let ((filename (car entry))
          (table watch-elc-files-file-times))
      ;; Map the name to the compiled file name, if the file has been
      ;; compiled.
      (when (not (s-ends-with? ".elc" filename))
        (let ((compiled-name (byte-compile-dest-file filename)))
          (when (file-exists-p compiled-name)
            (setq filename compiled-name))))

      ;; Look for files that have been changed since we started
      ;; watching.
      (when (file-exists-p filename)
        (let ((old-mtime (gethash filename table))
              (new-mtime (nth 5 (file-attributes filename))))
          (when (and old-mtime new-mtime
                     (time-less-p old-mtime new-mtime))
            ;; Reload the file asynchronously.
            ;; Adding :nosuffix here suppresses the loading message in
            ;; Emacs 24.3.1.
            (message "watch-elc-fils: file changed")
            (run-at-time nil nil 'load filename :noerror))
          (puthash filename new-mtime table))))))

(provide 'watch-elc-files)
