(eval-when-compile (require 'cl))
(require 'my-util)
;;(require 'jasmel)

;; (defadvice file-relative-name
;;   (before bugfix (filename &optional directory)
;;           activate)                     ; Do not compile!
;;   "Fix handling of symlinks.  This is needed to allow the vc
;; Subversion module to work correctly when accessing files through
;; symlinks."
;;   (ad-set-arg 0 (file-truename filename))
;;   (ad-set-arg 1 (file-truename (or directory
;;                                    default-directory))))

;;;###autoload
(defun my-move-file (old-name new-name use-vc-p make-target-dir-p)
  "Move a file and update any buffers visiting that file."
  (interactive (my-copy-or-move-args 'mv))
  (my-copy-or-move 'mv old-name new-name use-vc-p make-target-dir-p t))

;;;###autoload
(defun my-copy-file (old-name new-name use-vc-p make-target-dir-p update-buffers-p)
  "Copy a file and optionally update any buffers visiting that file."
  (interactive (append (my-copy-or-move-args 'cp)
                       (list (y-or-n-p "Point existing buffers to new name? "))))
  (my-copy-or-move 'cp old-name new-name use-vc-p make-target-dir-p update-buffers-p))

;;;###autoload
(defun my-delete-file (file-to-delete use-vc)
  "Delete a file and kill any buffers visiting that file.
Defaults to deleting the file in the current buffer."
  (interactive
   (let* ((name (buffer-file-name))
          (vc-backend (vc-backend name))
          (use-vc (and vc-backend
                       (y-or-n-p (format "Delete using %s? " vc-backend))))
          (confirm (or use-vc
                       (and (not (file-exists-p name))
                            (not (buffer-modified-p)))
                       (and vc-backend
                            (y-or-n-p (format "Really delete %s? " name)))
                       (yes-or-no-p (format "Really delete %s? " name)))))
     (list (and confirm name) use-vc)))
  (when file-to-delete
    (when (file-exists-p file-to-delete)
      ;; Delete the file from the file system.
      (case (and use-vc (vc-backend file-to-delete))
        (GIT (my-call-process (list "git" "rm"
                                    "--force"
                                    (file-name-nondirectory file-to-delete))
                              :dir (file-name-directory file-to-delete)
                              :check-status t))
        (SVN (my-call-process (list "svn" "rm"
                                    "--force" file-to-delete)
                              :check-status t))
        (t
         (unless (zerop (call-process "rm" nil nil nil file-to-delete))
           (error "External command failed.")))))
    ;; Kill the current buffer if it points to the file being deleted
    ;; and the file was actually deleted (or if it was never in the
    ;; file system to begin with.)  If there are any other buffers
    ;; visiting this file, leave them alone.
    (when (and file-to-delete
               (not (file-exists-p file-to-delete))
               (equal buffer-file-name file-to-delete))
      (kill-buffer))))

(defun my-make-executable ()
  "Set the execute bit(s) on the current file."
  (interactive)
  (when (eq major-mode 'fundamental-mode)
    (if (and (not (buffer-modified-p))
             (equal (point-min) (point-max)))
        (progn
          (insert "#! /bin/bash

set -e
here=$(dirname \"$0\")

die() {
echo \"$*\" >&2
exit 1
}
")

          (normal-mode)
          (indent-region (point-min) (point-max))
          (set-buffer-modified-p nil))
      (normal-mode)))
  (assert (buffer-file-name))
  (if (file-exists-p (buffer-file-name))
      (my-make-executable-1)
    (add-hook 'after-save-hook 'my-make-executable-after-save-hook)))


(defun my-make-executable-after-find-file-hook ()
  (add-hook 'after-find-file-hook 'my-make-executable-after-find-file-hook nil 'local)
  (remove-hook 'after-find-file-hook 'my-make-executable-after-find-file-hook))


(defun my-make-executable-after-save-hook ()
  (my-make-executable-1)
  (remove-hook 'after-save-hook 'my-make-executable-after-save-hook 'local))


(defun my-make-executable-1 ()
  ;; Find all the "read" bits that are set, and set the corresponding
  ;; "write" bits.
  (let ((mode (file-modes (buffer-file-name))))
    (setq mode (logior mode (lsh (logand mode #o444) -2)))
    (set-file-modes (buffer-file-name) mode)))


(defun my-copy-or-move-args (action)
  (let* ((op-name (if (eq action 'cp) "copy" "move"))
         (file (buffer-file-name))
         (file-exists (file-exists-p file))
         (dir (file-name-directory file))
         (nondir (file-name-nondirectory file))
         (old-name (expand-file-name
                    (buffer-file-name-or-read-file-name
                     (format "File to %s: " op-name) t)))
         (new-name (expand-file-name
                    (read-file-name "New filename: " nil file)))
         (old-backend (and file-exists (vc-backend old-name)))
         (use-vc-p (and old-backend
                        (y-or-n-p (format "%s using %s? "
                                          (capitalize op-name)
                                          old-backend))))
         (target-dir (file-name-directory new-name))
         (make-target-dir-p (and (not (file-exists-p target-dir))
                                 (y-or-n-p
                                  (format "Create directory %s? " target-dir)))))
    (list old-name new-name use-vc-p make-target-dir-p)))

(defun* my-copy-or-move (action
                         old-name
                         new-name
                         use-vc-p
                         make-target-dir-p
                         update-buffers-p)
  (assert (memq action '(cp mv)))
  (let* ((op-name (case action (cp "copy") (mv "move")))
         (cmd-name (case action (cp "cp") (mv "mv")))
         (old-backend (and use-vc-p (vc-backend old-name)))
         (new-backend (vc-backend new-name))

         new-dir)

    (cond
     ((string= new-name
               (file-name-as-directory new-name))
      (setq make-target-dir-p t
            new-dir new-name
            new-name (concat new-name
                             (file-name-nondirectory old-name))))

     ((file-directory-p new-name)
      (setq new-dir (file-name-as-directory new-name)
            new-name (concat new-dir
                             (file-name-nondirectory old-name))))

     (t (setq new-dir (file-name-directory new-name))))

    ;; Optionally create the target directory if it doesn't exist yet.
    (when (and make-target-dir-p
               (not (file-directory-p new-dir)))
      (make-directory new-dir t))


    ;; Check for an existing destination file.
    ;; TODO: move this to my-copy-or-move-args
    (when (or (and new-backend
                   (not (yes-or-no-p
                         "Destination is already under version control; continue? ")))
              (and (file-exists-p new-name)
                   (not (yes-or-no-p
                         (format "Destination file (%s) already exists; overwrite it? " new-name)))))
      (message "Aborted.")
      (return-from my-copy-or-move))

    ;; Do the copy or move.
    (when (file-exists-p old-name)
      (case old-backend
        (Git (let ((rel-old-name (file-relative-name old-name))
                   (rel-new-name (file-relative-name new-name)))
               (case action
                 (cp
                  (my-call-process (list "cp" rel-old-name rel-new-name)
                                   :check-status t)
                  (my-call-process (list "git" "add" rel-new-name)
                                   :check-status t))
                 (mv
                  (my-call-process (list "git" "mv" "-f" rel-old-name rel-new-name)
                                   :check-status t)))))
        (SVN (my-call-process (list "svn" cmd-name
                                    "--force" old-name new-name)
                              :check-status t))
        ((nil) (my-call-process (list cmd-name old-name new-name)
                                :check-status t))
        (t (error "Cannot %s using %s backend."
                  op-name old-backend))))

    (when update-buffers-p
      ;; Update buffers visiting OLD-NAME to point to NEW-NAME.
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (equal (buffer-file-name) old-name)
            (let ((modified (buffer-modified-p)))
              (set-visited-file-name new-name t t)
              (set-buffer-modified-p modified))))))))

(defun* my-call-process (program-and-args
                         &key
                         infile buffer display
                         start end delete
                         shell check-status
                         dir)
  "Wrapper for functions that call external processes.

If START is non-nil, `call-process-region' is called.  If SHELL is non-nil, `call-process-shell-command is called.  Otherwise `call-process' is called.  The arguments PROGRAM, ARGS, INFILE, BUFFER, DISPLAY, START, END and DELETE are passed through unchanged to the underlying function.  Unused arguments must be nil.

If CHECK-STATUS is t, an error is generated if the external process returns a nonzero status code."
  (let ((program (car program-and-args))
        (args (cdr program-and-args))
        (default-directory (or dir default-directory)) ;; override global
        proc-status)

    ;; Invoke the process.
    (if start
        (progn
          (assert (null infile))
          (assert (not shell))
          (setq proc-status
                (apply #'call-process-region
                       start end program delete buffer display args)))
      (assert (not end))
      (assert (not delete))
      ;;(message "calling process: %S" (cons program args))
      (setq proc-status
            (apply (if shell #'call-process-shell-command #'call-process)
                   program infile buffer display args)))

    ;; Check the status code.
    (if (and check-status
             (not (zerop proc-status)))
        (error "Process returned %s: %s"
               proc-status (mapconcat #'identity (cons program args) " ")))

    ;; Return the exit code.
    proc-status))

(defun buffer-file-name-or-read-file-name (prompt &optional mustmatch)
  "Read a file name with `read-file-name', but use
`buffer-file-name' as the default if the current buffer has a
filename."
  (if (and (buffer-file-name)
           (not current-prefix-arg))
   (buffer-file-name)
   (read-file-name prompt nil nil mustmatch)))

(provide 'my-files)
