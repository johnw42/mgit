;; -*- no-byte-compile: t -*-
(require 'my-syntax)
(require 'python)
(require 'my-dabbrev)
(require 'my-local)
;;(require 'my-imports-python)

;;;
;;; Python mode settings.
;;;

;; (progn
;;   (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;   ;; Support mode detection for Python interpreters that include a
;;   ;; version in their name.
;;   (dolist (major '(1 2 3))
;;     (dolist (minor '(0 1 2 3 4 5 6 7 8 9))
;;       (add-to-list 'interpreter-mode-alist
;;                    (cons (format "python%s.%s" major minor) 'python-mode)
;;                    'append))))

(defun my-case-sensitive-dabbrevs ()
  "Expand dynamic abbreviations in a case-sensitive manner."
  (local-setq dabbrev-case-fold-search nil)
  (local-setq dabbrev-case-replace nil))

(add-hook
 'python-mode-hook
 (defun mypy-mode-hook ()

   (setq last-kbd-macro
         [?\M-\; ?\C-b ?\C-k ?  f3])

   ;; (add-to-list (make-local-variable 'after-change-functions)
   ;;              'mypy-auto-self)
   (local-set-key ":" 'mypy-electric-colon)
   (local-set-key "(" 'mypy-electric-lparen)
   (local-set-key ")" 'mypy-electric-rparen)
   (local-set-key "." 'mypy-electric-dot)
   (local-set-key (kbd "C-c i") 'my-imports-interactive-import)
   (set (make-local-variable 'fill-paragraph-function) 'mypy-fill-paragraph)
   ;;(local-set-key (kbd "C-q") 'mypy-fill-paragraph)

   ;; (font-lock-add-keywords
   ;;  nil
   ;;  '(("\\bthis\\b" font-lock-warning-face)))

   ;; (local-set-key (kbd "<tab>") 'mypy-indent-region)
   ;; (local-set-key (kbd "S-<tab>") 'mypy-unindent-region)
   ;; (dotimes (i 256)
   ;;   (when (or (and (<= ?a i) (<= i ?z))
   ;;             (and (<= ?A i) (<= i ?Z))
   ;;             (and (<= ?0 i) (<= i ?9))
   ;;             (= i ?_))
   ;;     (local-set-key (string i) 'self-insert-command)))
   (setq truncate-lines t)))

(add-hook 'python-mode-hook 'my-case-sensitive-dabbrevs)
(setq-default py-indent-offset 4)

(defadvice python-calculate-indentation (after my-python activate)
  "Arrange for the closing delimiter of a docstring to line up
with the opening delimiter."
  (let (docstring-delimiter starting-point)
    (and (save-excursion
           (beginning-of-line)
           (when (looking-at (rx (0+ " ") (group (or "'''" "\"\"\"")) eol))
             (setq docstring-delimiter (match-string 1))
             t))
         (save-excursion
           (python-beginning-of-statement)
           (when (looking-at-p (rx-to-string (eval docstring-delimiter)))
             (setq ad-return-value (current-column)))))))

(defun mypy-docstring-range (&optional pos)
  "Finds the boundaries of the current docstring.

  If POS (default: (point)) is in a docstring, returns a
  list (START END) containing the limits of the
  docstring (including the delimiters).  If POS is not in a
  docstring, returns nil."
  (let* ((docstring-start (syntax-ppss pos)))
    (when (and docstring-start
               (save-excursion
                 (goto-char docstring-start)
                 (save-match-data
                   (looking-at "\"\"\""))))
      (let ((docstring-end (save-excursion
                             (goto-char docstring-start)
                             (forward-sexp)
                             (point))))
        (list docstring-start docstring-end)))))

(defun mypy-narrow-to-docstring (&optional pos)
  "Narrows the current buffer to a docstring.

  If POS (default: (point)) is inside a docstring, returns
  narrows the region to just the docstring (including delimiters)
  and returns t.  If POS is not in a docstring, does nothing and
  returns nil."
  (let ((range (mypy-docstring-range pos)))
    (when range
      (narrow-to-region (car range)
                        (cadr range))
      t)))

(defun mypy-line-indent ()
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (looking-at "\\(\\s-*\\)")
      (match-string 1))))

(defun mypy-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (and (looking-at "\\s-*$")
           t))))

;; (defun mypy-fill-paragraph (justify)
;;   ;; (save-restriction
;;   ;;   (when (and (mypy-narrow-to-docstring)
;;   ;;              (not (mypy-line-empty-p)))
;;   ;;     (let* ((indent-here (mypy-line-indent))
;;   ;;            (docstring-indent (save-excursion
;;   ;;                                (goto-char (point-min))
;;   ;;                                (mypy-line-indent)))

;;   ;;            (paragraph-start (save-excursion
;;   ;;                               (beginning-of-line)
;;   ;;                               (let ((start (point)))
;;   ;;                                 (while (not (or (mypy-line-empty-p)
;;   ;;                                                 (= (point) (point-min))))
;;   ;;                                   (setq start (point))
;;   ;;                                   (forward-line -1))
;;   ;;                                 start)))
;;   ;;            (paragraph-end (save-excursion
;;   ;;                             (forward-line)
;;   ;;                             (while (not (or (= (point) (point-max))
;;   ;;                                             (mypy-line-empty-p)))
;;   ;;                               (forward-line))
;;   ;;                             (point)))

;;   ;;            (section-start (save-excursion
;;   ;;                             (beginning-of-line)
;;   ;;                             (let ((start (point)))
;;   ;;                               (while (and (not (mypy-line-empty-p))
;;   ;;                                           (> (point) (point-min)))
;;   ;;                                 (setq start (point))
;;   ;;                                 (forward-line -1)))
;;   ;;                             ))

;;   ;;            (indent-pattern (regexp-quote indent))
;;   ;;            (section-start (save-excursion
;;   ;;                             (beginning-of-line)
;;   ;;                             (let ((start (point)))
;;   ;;                               (while (and (not (looking-at "\\s*$"))
;;   ;;                                           (> (point) (point-min)))
;;   ;;                                 (setq start (point))
;;   ;;                                 (forward-line -1)))
;;   ;;                             ))
;;   ;;            (heading-start
;;   ;;             (save-excursion
;;   ;;               (beginning-of-line)
;;   ;;               (while (and (> (point) (point-min))
;;   ;;                           (not (looking-at (concat indent-pattern
;;   ;;                                                    "[A-Z][a-zA-Z]*:$"))))
;;   ;;                 (forward-line -1))
;;   ;;               (when (> (point) (point-min))
;;   ;;                 (point)))))

;;   ;;       )
;;   ;;     ))
;;   ;; (let ((docstring-range (mypy-docstring-range)))
;;   ;;   (when docstring-range
;;   ;;     (let ((docstring-start (car docstring-range))
;;   ;;           (docstring-end (cadr docstring-range))))
;;   ;;     ;; Find the nearest header line.
;;   ;;     ;; If we're indented 2 spaces from the header line,
;;   ;;     )
;;   ;;   ))
;;   nil)

(defun mypy-logical-line ()
  (save-excursion
    (let ((here (point)))
      ;; (while (and (not (bobp))
      ;;             (progn
      ;;               (forward-line -1)
      ;;               (not (looking-at "\\s-*\\(class\\|def\\|return\\|yield\\|raise\\|if\\|while\\|for\\|with\\|import\\|del\\|try\\|except\\|finally\\)"))))))))
      (while (condition-case err
                 (prog1 t (backward-up-list))
               (scan-error nil)))
      )))

(defun mypy-at-line-start-p ()
  "Test whether (point) is at the start of a line (ignoring
indentation)."
  (<= (point)
      (save-excursion
        (back-to-indentation)
        (point))))

(defun mypy-at-logical-line-start-p ()
  "Test whether (point) is at the start of a logical
line (ignoring indentation)."
  (and (mypy-at-line-start-p)
       (condition-case err
           (save-excursion
             (backward-up-list)
             nil)
         (scan-error t))
       (save-excursion
         (forward-line -1)
         (not (looking-at ".*\\\\\\s-*$")))))

;; (defun mypy-self-insert-command (n)
;;   "Expand \".\" to \"self.\" in Python code."
;;   (interactive "p")
;;   (self-insert-command n)
;;   (when (syntax-normal-p)
;;     (cond
;;      ((looking-back "\\(\\s-\\|[[({]\\)\\.\\.[a-zA-Z_]")
;;       (let ((class (mypy-current-class)))
;;         (when class
;;           (save-excursion
;;             (backward-char 2)
;;             (backward-delete-char 1)
;;             (insert class)))))
;;      ((and (looking-back "\\(\\s-\\|[[({]\\)\\.[a-zA-Z_]")
;;            (save-excursion
;;              (backward-char 2)
;;              (or (not (mypy-at-line-start-p))
;;                  (mypy-at-logical-line-start-p))))
;;       (save-excursion
;;         (backward-char 2)
;;         (insert "self"))))))

(defun mypy-script ()
  (interactive)
  (python-mode)
  (mypy-file-not-found)
  (save-buffer)
  (call-process "chmod" nil nil nil
                "+x" (buffer-file-name)))

(defun my-replace-all (text to-string-or-func)
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (while (search-forward text nil t)
      (let ((to-string (if (functionp to-string-or-func)
                           (funcall to-string-or-func)
                         to-string-or-func)))
        (replace-match to-string t t)))))

(defun mypy-insert-file-template (text)
  (assert (string-match-p "^\n" text))
  (delete-region (point-min) (point-max))
  (insert (substring text 1) "\n")
  (my-replace-all "^\(  \)+"
                  (lambda ()
                    (make-string (* python-indent-offset
                                    (/ (- (match-end 0)
                                          (match-beginning 0))
                                       2))
                                 ? )))
  (my-replace-all "<CURSOR>" ""))

(defun mypy-file-not-found ()
  (set-auto-mode t)
  (when (eq major-mode 'python-mode)
    (when (equal (file-truename (file-name-directory (or buffer-file-name "")))
                 (file-truename (expand-file-name "~/bin/")))
      )
    ;;       (my-insert-file-template
    ;;        "#! /usr/bin/env python2.6
    ;; # -*- mode: python; py-indent-offset: 4 -*-
    ;; from __future__ import print_function
    ;; from __future__ import absolute_import
    ;; from __future__ import division
    ;; import sys
    ;; import logging

    ;; LOG = logging.getLogger(__name__)

    ;; def main():
    ;;     logging.basicConfig(level=logging.INFO)
    ;;     <CURSOR>

    ;; if __name__ == \"__main__\": main()
    ;; ")
    (not-modified)))

(defun mypy-doc (name)
  (interactive "sName to look up: ")
  (pop-to-buffer "*pydoc*")
  (kill-all-local-variables)
  (erase-buffer)
  (call-process "pydoc" (not 'infile) t (not 'display) name)
  (view-mode 1)
  (goto-char (point-min)))

(defun mypy-in-class ()
  (eq (caar (mypy-context)) 'class))

(defun mypy-self-var ()
  (if (not (mypy-in-class)) nil
    (save-excursion
      (forward-line 0)
      (save-match-data
        (looking-at "\\s-*def\\s-+\\([a-zA-Z0-9_]+\\)")
        (let ((method-name (match-string 1)))
          (if (equal "__new__" method-name) "cls"
            (forward-line -1)
            (if (looking-at "\\s-*@staticmethod\\b") nil
              (if (looking-at "\\s-*@classmethod\\b") "cls"
                "self"))))))))

(defun mypy-electric-colon (n)
  "Add a minimal argument list to a function declaration."
  (interactive "p")
  (when (and (= n 1)
             (save-excursion
               (move-beginning-of-line 1)
               (looking-at "\\s-*def\\s-+[a-zA-Z0-9_]+$")))
    (let ((self-var (mypy-self-var)))
      (insert "(" (or self-var "") ")")))
  (self-insert-command n))

(defun mypy-electric-lparen (n)
  "Add a \"self\" argument to a function declaration."
  (interactive "p")
  (self-insert-command n)
  (when (and (= n 1)
             (save-excursion
               (move-beginning-of-line 1)
               (looking-at "\\s-*def\\s-+[a-zA-Z0-9_]+($")))
    (let ((self-var (mypy-self-var)))
      (when self-var
        (insert self-var ", ")))))

(defun mypy-electric-rparen (n)
  "Delete an extraneous comma inserted by MYPY-ELECTRIC-LPAREN."
  (interactive "p")
  (when (looking-back ", *$")
    (replace-match ""))
  (self-insert-command n))

(defun mypy-electric-dot (n)
  "Expand \"super\" to \"super(<class>, self)\"."
  (interactive "p")
  (when (looking-back "\\bsuper\\b")
    (insert "(" (mypy-current-class)
            ", self)"))
  (self-insert-command n))

;; (defun mypy-expand-at (begin end length)
;;   "Function to expand @var into self.var in Python code."
;;   (save-match-data ; needed to avoid breaking dabbrev-expand
;;     (when (and (looking-at "\\_>")
;;                (save-excursion (backward-char 2)
;;                                (looking-at "\\B@"))
;;                (eq (car (mypy-context)) 'def))
;;       (backward-char 1)
;;       (delete-backward-char 1)
;;       (insert "self.")
;;       (forward-char 1))))

(defun my-leading-whitespace ()
  "Return the leading whitespace of the current line."
  (save-excursion (beginning-of-line)
                  (search-forward-regexp "\\s-*")
                  (match-string 0)))

(defun mypy-find-controlling-line ()
  "Move the the start of the first line above this line with less
indentation."
  (let ((indent (length (my-leading-whitespace))))
    (while (and (/= (point) (point-min))
                (or (>= (length (my-leading-whitespace))
                        indent)
                    (looking-at "\\s-*$")))
      (forward-line -1))))

(defun mypy-controlling-line ()
  (save-excursion
    (mypy-find-controlling-line)
    (thing-at-point 'line)))

(defun mypy-context ()
  (save-excursion
    (mypy-find-controlling-line)
    (if (= (point) (point-min))
        '((nil ""))
      (let ((line (thing-at-point 'line))
            (tail (mypy-context)))
        (looking-at "\\s-*\\([a-z]+\\)\\s-+\\([a-zA-Z0-9_]+\\)")
        (let ((keyword (match-string 1))
              (name (match-string 2)))
          (message "kw %s" keyword)
          (cond
           ((equal keyword "class")
            (cons (cons 'class name) tail))
           ((equal keyword "def")
            (cons (cons 'def name) tail))
           (t tail)))))))

(defun mypy-current-class ()
  (catch t
    (let ((context (mypy-context)))
      (while context
        (if (eq 'class (caar context))
            (throw t (cdar context)))
        (pop context)))
    ""))

(defun mypy--fill-comment-suite ()
  "Fills a \"suite\" of comments in a Python docstring.  Assumes
  point is at the start of the first line of the suite.  Leaves
  point at the start of the first line after the suite."

  (assert (bolp))
  (cond
   ((looking-at "\\( *\\)[^:\n]*:\n\\1 ")
    ;; Handle a section with a header like "Args:", "Returns:", etc.
    (let ((suite-indent (match-string 1)))
      (let ((fill-prefix (concat suite-indent "  "))
            block-start)
        (forward-line 1)
        (setq block-start (point))
        (while (and (< (point) (point-max))
                    (looking-at "\\(\s+\\)")
                    (> (length (match-string 1))
                       (length suite-indent)))
          (forward-line 1))
        (save-restriction
          (narrow-to-region block-start (point))
          (goto-char (point-min))
          (mypy--fill-comment-suite))
        t)))

   ((looking-at "\\( *\\)[^ :\n]*: *[^ \n]*")
    ;; Handle the documentation for an individual variable, attribute,
    ;; etc.
    (let* ((suite-indent (match-string 1))
           (fill-prefix (concat suite-indent "    "))
           (block-start (point)))
      (forward-line 1)
      (while (and (< (point) (point-max))
                  (looking-at "\\(\s+\\)")
                  (> (length (match-string 1))
                     (length suite-indent)))
        (forward-line 1))
      (fill-region-as-paragraph block-start (point))
      t))

   (t
    ;; Handle an ordinary paragraph.
    (fill-region-as-paragraph (point-min) (point-max))
    nil)))

(defun mypy-fill-paragraph (_)
  "Value of `fill-paragraph-function' for Python code."
  (interactive)
  (let ((docstring-start (nth 8 (syntax-ppss)))
        doc-start doc-end
        para-end)
    (when (and docstring-start
               (save-excursion
                 (goto-char docstring-start)
                 (beginning-of-line)
                 (when (looking-at " *\"\"\"")
                   (setq doc-start (point))
                   (forward-sexp)
                   (setq doc-end (- (point) 3))
                   t)))
      (save-restriction
        (narrow-to-region doc-start doc-end)
        (forward-paragraph)
        (setq para-end (point))
        (backward-paragraph)
        (narrow-to-region (point) para-end)
        (if (equal (point) doc-start)
            (fill-region-as-paragraph (point) para-end)
          (forward-line)
          (while (and (< (point) (point-max))
                      (mypy--fill-comment-suite)))))
      t)))

(defun mymy-number-lines-in-region (begin end)
  (interactive "r")
  (setq end (copy-marker end))
  (let ((counter 1))
    (goto-char begin)
    (while (< (point) end)
      (comment-indent)
      (delete-region (point) (line-end-position))
      (insert (if (= 1 counter)
                  "---"
                (format "%s" counter)))
      (incf counter)
      (forward-line))))

(defvar pydoc-program "pydoc")

(defun pydoc (topic)
  (interactive "sTopic: ")
  (let ((manual-program pydoc-program))
    (man topic)))

(provide 'my-python)

;; Local Variables:
;; no-byte-compile: t
;; End:
