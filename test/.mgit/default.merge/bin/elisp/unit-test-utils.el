;; -*- no-byte-compile: t -*-
(require 'my-load-path)

(defun unit-test-file-p (file-name)
  "Returns non-nil when FILE-FILE is the name of a test file."
  (and file-name
       (let ((case-fold-search nil))
         (string-match-p "-test\\.el$" file-name))))

(defun unit-test-test-file (file-name)
  "Returns the name of the unit test file for the file named
FILE-NAME."
  (save-match-data
    (let ((case-fold-search nil))
      (when (string-match-p "\\.el$" file-name)
        (replace-regexp-in-string "\\(-test\\)?\\.el$" "-test.el" file-name)))))

(defun unit-test-tested-file (file-name)
  "When FILE-NAME is a unit test file, returns the name of the
corresponding non-test file."
  (save-match-data
    (let ((case-fold-search nil))
      (when (string-match-p "\\.el$" file-name)
        (replace-regexp-in-string "\\(-test\\)?\\.el$" ".el" file-name)))))

(defun unit-test-feature-name (file-name)
  (let ((case-fold-search nil))
    (save-match-data
      (let ((basename (file-name-nondirectory file-name)))
        (when (string-match "\\(-test\\)?\\.el$" basename)
          (substring basename 0 (match-beginning 0)))))))

(defun unit-test-find-file-not-found-function ()
  (when (in-load-path-p buffer-file-name)
    (let ((feature-name (unit-test-feature-name buffer-file-name)))
      (if (unit-test-file-p buffer-file-name)
          (save-excursion
            (message "Inserting text from unit-test-find-file-not-found-function.")
            (insert "(require '"
                    feature-name
                    ")\n(require 'unit-test-core)\n\n(unit-test-suite "
                    feature-name
                    "-test)\n"))
        (save-excursion
          (message "Inserting text from unit-test-find-file-not-found-function.")
          (insert ";; -*- lexical-binding: t -*-\n")
          (save-excursion
            (insert "\n\n(provide '" feature-name ")\n")))))
    (not-modified)
    t))

(add-hook 'find-file-not-found-functions
          'unit-test-find-file-not-found-function)

;; (defun unit-test-suite-abbrev-hook ()
;;   (when (and (looking-back-p "(unit-test-suite")
;;              (string-suffix-p ".el" buffer-file-name))
;;     (insert " "
;;             (string-without-suffix
;;              ".el" (file-name-nondirectory buffer-file-name)))))

;; ;; Define an abbrev for "unit-test-suite" that automatically inserts
;; ;; the current module name.
;; (define-abbrev
;;   lisp-mode-abbrev-table
;;   "suite"
;;   t
;;   'unit-test-suite-abbrev-hook
;;   :system t
;;   :case-fixed t)

(provide 'unit-test-utils)

;; Local Variables:
;; no-byte-compile: t
;; End:
