;; -*- no-byte-compile: t -*-
(require 'cl)
(require 'my-google)
(require 'my-hooks)
(require 'my-background-update)
(require 'my-syntax)

(defun my-go-mode-hook ()
  (add-hook 'post-self-insert-hook my-go-post-self-insert-hook)
  (setq 'my-imports-parse-import-line-function
        'my-go-parse-import-line))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(add-file-hook 'after-save-hook
               'go-after-save-hook
               :mode 'go-mode)

(defun go-after-save-hook ()
  ;;(schedule-background-update 'async-gofmt)
  (gofmt)
  (when (google3-buffer-p)
    (let* ((g3-subdir (g3-subdir))
           (default-directory (g3-dir)))
      (start-process "glaze" nil "glaze" g3-subdir))))

(defun async-gofmt ()
  (start-file-process "gofmt" nil
                      "gofmt" "-w" (buffer-file-name)))

(defun gofmt ()
  (interactive)
  (save-buffer)
  (message "Formatting...")
  (process-file "gofmt"
                nil
                nil
                nil
                "-w"
                buffer-file-name)
  (revert-buffer)
  (message "Formatting...done."))

(defun my-go-post-self-insert-hook ()
  (let ((case-fold-search nil))
    (save-match-data
      (when (looking-back "\\(^\\|[^A-Za-z0-9_]\\)\\([a-z0-9]+\\)\\.\\([A-Z][A-Za-z0-9]*\\)[^A-Za-z0-9_]")
        (message "let's add an import for %s" (match-string 2))
        (save-excursion
          (backward-char)

          )))))

(defun my-go-parse-import-line (line)
  )

(provide 'my-go)

;; Local Variables:
;; no-byte-compile: t
;; End:
