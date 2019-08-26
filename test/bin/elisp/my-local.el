;; -*- no-byte-compile: t -*-
;; This file implements local configuration.  Each file a file is
;; visited, a hook is called to search up the directory hierarchy for
;; a file named .local.el (configurable via
;; my-local-config-file-name), and the contents of the file are
;; executed.
;;
;; To prevent global variables from being accidentally clobbered, setq
;; is bound to local-setq during evaluation of local config files.

(defcustom my-local-config-file-name ".local.el"
  "The name of local configuration files.")

(defmacro local-setq (&rest body)
  "Like `setq' except that each SYM is made into a local variable.
  During evaluation of local config files, `setq' is aliased to this
  macro.

\(fn SYM VAL SYM VAL ...)"
  `(progn ,@(reverse
             (let (args)
               (while body
                 (push `(set (make-local-variable ',(pop body))
                             ,(pop body)) args))
               args))))


(add-hook 'find-file-hook 'my-local-find-file-hook)
(defun my-local-find-file-hook ()
  "Hook function used to load local configuration from a file named
`my-local-config-file-name'."
  (when (buffer-file-name)
    (my-local-run-config (file-name-directory (buffer-file-name)))))

(defun my-local-run-config (local-directory)
  "This function implements loading of local config files."
  (when local-directory
    (let ((parent (file-name-directory
                   (directory-file-name local-directory))))
      (unless (equal parent local-directory)
        (my-local-run-config parent)))
    (let ((default-directory local-directory))
      (when (and (file-readable-p my-local-config-file-name)
                 (file-regular-p my-local-config-file-name))
        (message "loading local settings for %s" default-directory)
        (let ((original-setq (symbol-function 'setq)))
          (fset 'setq (symbol-function 'local-setq))
          (unwind-protect
              (load my-local-config-file-name 'noerror 'nomessage)
            (fset 'setq original-setq)))))))

(add-hook 'find-file-not-found-functions 'my-local-file-not-found-hook)
(defun my-local-file-not-found-hook ()
  "This function inserts some template code when a local config
  file is created."
  (when (equal my-local-config-file-name
               (file-name-nondirectory (buffer-file-name)))
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; Variables set with setq become buffer-local.\n"
     ";;\n"
     ";; Example:\n"
     ";; (when (eq major-mode 'java-mode)\n"
     ";;   (setq tab-width 8\n"
     ";;         c-basic-offset 2))\n")
    t))

(provide 'my-local)

;; Local Variables:
;; no-byte-compile: t
;; End:
