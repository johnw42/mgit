;; -*- no-byte-compile: t -*-
(require 'cl-macs)
(require 'dash)
(require 'my-util) ;; for point-after

(defun parse-unified-diff-buffer (buffer)
  "Parse the output of diff -u from BUFFER.

The output is list containing an alist for each pair of files
there were compared.  The alist for each pair potentially has the
following keys:

  :old-path - The path of the old file (from the \"---\" line)
  :new-path - The path of the new file (from the \"+++\" line)
  :old-missing-newline - Present with the value t if the old
      file is does *not* end with a newline character.
  :new-missing-newline - Present with the value t if the new
      file is does *not* end with a newline character.
  :chunk - A list of alists.

The value of the :chunk entry is a list of alists, one for each
modified chunk.  Each chunk alist has the following keys:

  :old-start - The line number of the start of the chunk in the
      old file.
  :old-length - The number of lines in the chunk in the old file.
  :new-start - The line number of the start of the chunk in the
      new file.
  :new-length - The number of lines in the chunk in the new file.
  :lines - The lines of the chunk.
"
  (let (file-pairs
        old-path
        old-missing-newline
        new-path
        new-missing-newline
        old-start
        old-length
        new-start
        new-length
        in-chunk
        chunks
        lines
        line-type
        old-line-number
        new-line-number)
    (cl-labels ((end-of-chunk
                 ()
                 (when in-chunk
                   (push (list :old-start old-start
                               :old-length old-length
                               :new-start new-start
                               :new-length new-length
                               :lines (nreverse lines))
                         chunks))
                 (setq lines nil
                       in-chunk nil))
                (end-of-file-pair
                 ()
                 (end-of-chunk)
                 (when chunks
                   (push (list :old-path old-path
                               :new-path new-path
                               :old-missing-newline old-missing-newline
                               :new-missing-newline new-missing-newline
                               :chunks (nreverse chunks))
                         file-pairs)
                   (setq chunks nil))
                 (setq old-missing-newline nil
                       new-missing-newline nil)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (while (not (eobp))
          (cond
           ((and in-chunk (looking-at "^\\\\ No newline at end of file$"))
            (case line-type
              (:added (setq new-missing-newline t))
              (:deleted (setq old-missing-newline t))))
           ((and in-chunk (looking-at "^\\([-+ ]\\)\\(.*\\)$"))
            (setq line-type
                  (case (string-to-char (match-string 1))
                    (?  :context)
                    (?+ :added)
                    (?- :deleted)))
            (push (list :type line-type
                        :old-line old-line-number
                        :new-line new-line-number
                        :content (match-string 2))
                  lines)
            (when (memq line-type '(:context :deleted))
              (incf old-line-number))
            (when (memq line-type '(:context :added))
              (incf new-line-number)))
           ((looking-at "^@@ -\\([0-9]+\\)\\(,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(,\\([0-9]+\\)\\)? @@$")
            (end-of-chunk)
            (setq in-chunk t
                  old-start (string-to-number (match-string 1))
                  old-line-number old-start
                  old-length (string-to-number (or (match-string 3) "1"))
                  new-start (string-to-number (match-string 4))
                  new-line-number new-start
                  new-length (string-to-number (or (match-string 6) "1"))))
           ((looking-at "^--- \\([^\t]+\\)\t")
            (end-of-file-pair)
            (setq old-path (match-string 1)))
           ((looking-at "^\\+\\+\\+ \\([^\t]+\\)\t")
            (setq new-path (match-string 1))))
          (forward-line))
        (end-of-file-pair)))
    (nreverse file-pairs)))

(defun diff-buffer-against-file (&optional filename)
  "Diff the contents of a buffer against a file and return the
resulting output of `parse-unified-diff-buffer'."
  (setq filename (or filename
                     (buffer-file-name)))
  (when filename
    (let ((temp-buffer (generate-new-buffer " *diff*")))
      (unwind-protect
          (progn
            (call-process-region (point-min)
                                 (point-max)
                                 "diff"
                                 nil
                                 temp-buffer
                                 nil
                                 "--unified=0"
                                 "-"
                                 filename)
            (parse-unified-diff-buffer temp-buffer))
        (kill-buffer temp-buffer)))))

(defun diff-revert--check-line (expected-content)
  "Check that the current line matches the string EXPECTED-CONTENT."
  (let ((end (+ (point)
                (length expected-content))))
    (unless (and (equal expected-content
                        (buffer-substring (point)
                                          (min (point-max) end)))
                 (or (= (point-max) end)
                     (equal "\n"
                            (buffer-substring end (1+ end)))))
      (error "Unexpectd buffer content."))))

;; (defmacro ensure-symbols (symbols sexpr)
;;   (declare (indent 1))

;;   (dolist (symbol symbols)
;;     `(progn
;;        (unless (symbolp ,symbol)
;;          (setq ,sexp `(let (()))))
;;        ,sexp)))

(defmacro diff-revert--with-whole-file (&rest body)
  (declare (indent 0))
  `(save-excursion
     (save-restriction
       (widen)
       ,@body)))

(cl-defmacro diff-revert--do-chunk-lines ((line-var diff) &rest body)
  (declare (indent 1))
  (let ((chunk (cl-gensym "chunk"))
        (diff-expr diff)
        (diff (cl-gensym "diff")))
    `(let ((,diff ,diff-expr))
       (assert (= 1 (length ,diff)))
       (diff-revert--with-whole-file
        (dolist (,chunk (plist-get (car ,diff) :chunks))
          (goto-char (point-min))
          (forward-line (+ (plist-get ,chunk :new-start)
                           (if (zerop (plist-get ,chunk :new-length))
                               0
                             -1)))
          (dolist (,line-var (plist-get ,chunk :lines))
            ,@body))))))

(defun diff-revert--check-diff (diff)
  (diff-revert--do-chunk-lines (line diff)
    (cl-destructuring-bind (&key type content) line
      (case type
        ((:context :deleted)
         (diff-revert--check-line content)
         (forward-line))))))

(defun diff-revert--apply-diff (diff)
  (diff-revert--with-whole-file
   (diff-revert--check-diff diff)
   (diff-revert--do-chunk-lines (line diff)
     (case (plist-get line :type)
       (:context
        (forward-line))
       (:deleted
        (let ((line-start (point)))
          (forward-line)
          (delete-region line-start (point))))
       (:added
        (insert (plist-get line :content) "\n"))))
   (cl-destructuring-bind
       (&key old-missing-newline new-missing-newline)
       (car diff)
     (cond
      ((and old-missing-newline
            (not new-missing-newline))
       (goto-char (point-max))
       (insert "\n"))
      ((and (not old-missing-newline)
            new-missing-newline)
       (goto-char (point-max))
       (backward-delete-char 1))))))


;; (defun diff-revert--apply-diff (diff)
;;   (assert (= 1 (length diff)))
;;   (diff-revert--check-diff diff)
;;   (let ((file-pair (car diff)))
;;     (save-excursion
;;       (save-restriction
;;         (widen)
;;         (dolist (chunk (plist-get file-pair :chunks))
;;           (goto-char (point-min))
;;           (forward-line (+ (plist-get chunk :new-start)
;;                            (if (zerop (plist-get chunk :new-length))
;;                                0
;;                              -1)))
;;           (dolist (line (plist-get chunk :lines))
;;             (let ((line-type (plist-get line :type))
;;                   (line-content (plist-get line :content)))
;;               (case line-type
;;                 (:context
;;                  (forward-line))
;;                 (:deleted
;;                  (let ((line-start (point)))
;;                    (forward-line)
;;                    (delete-region line-start (point))))
;;                 (:added
;;                  (insert (plist-get line :content) "\n"))))))
;;         (cond
;;          ((and (plist-get file-pair :old-missing-newline)
;;                (not (plist-get file-pair :new-missing-newline)))
;;           (goto-char (point-max))
;;           (insert "\n"))
;;          ((and (not (plist-get file-pair :old-missing-newline))
;;                (plist-get file-pair :new-missing-newline))
;;           (goto-char (point-max))
;;           (backward-delete-char 1)))))))

(defun diff-revert-buffer ()
  "Update this buffer to reflect the contents of its file.  This
function is like `revert-buffer', but it preserves the undo
history."
  (interactive)
  (let ((diff (diff-buffer-against-file)))
    (if (not diff)
        ;; No changes, just mark the buffer unmodified.
        (set-buffer-modified-p nil)
      (let ((inhibit-read-only t))
        (diff-revert--apply-diff diff)
      (unless (diff-buffer-against-file)
        (set-buffer-modified-p nil))))))

(provide 'diff-revert)

;; Local Variables:
;; no-byte-compile: t
;; End:
