;; -*- no-byte-compile: t -*-
(require 'shell)

;; When doing expansion on shell commands, expand !^ to the relative
;; name of the current buffer's file name and expand the relative name
;; to the absolute path.
(add-to-list
 'shell-dynamic-complete-functions
 (defun my-minibuffer-complete-shell-command ()
   (interactive)
   (let ((name (with-current-buffer (second (buffer-list))
                  (buffer-file-name))))
     (when name
       (let ((rel-name (and name (file-relative-name name))))
         (cond
          ((looking-back "!^" 2)
           (backward-delete-char 2)
           (insert rel-name)
           t)

          ((looking-back (concat "[^/]" (regexp-quote rel-name))
                         (1+ (length rel-name)))
           (backward-delete-char (length rel-name))
           (insert name)
           t)))))))

(provide 'my-shell)

;; Local Variables:
;; no-byte-compile: t
;; End:
