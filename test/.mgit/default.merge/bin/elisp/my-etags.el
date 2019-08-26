;; -*- no-byte-compile: t -*-
(require 'etags)
(require 'my-util)

(defun tags-files ()
  "Returns a list of tags files in use."
  (or (list tags-file-name)
      (mapcar (lambda (path)
                (if (file-directory-p path)
                    (concat file-directory-p "TAGS")
                  path))
              tags-table-list)))

(defun tags-update ()
  (interactive)
  (dolist (tags-file (tags-files))
    (let ((update-script (concat tags-file "-update")))
      (when (file-executable-p update-script)
        (message "Updating %S" tags-file)
        (let ((default-directory (file-name-directory tags-file)))
          (call-process update-script))))))

(provide 'my-etags)

;; Local Variables:
;; no-byte-compile: t
;; End:
