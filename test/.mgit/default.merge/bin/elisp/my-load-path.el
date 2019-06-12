;; -*- no-byte-compile: t -*-
(require 'cl)

(defun in-load-path-p (file-name)
  "Returns non-nil if FILE-NAME is an Emacs Lisp file in a
directory in `load-path'."
  (catch 'found
    (let ((case-fold-search nil))
      (when (string-match-p "\\.elc?$" file-name)
        (member (file-name-directory file-name) load-path)))))

(defun in-load-history-p (file-name)
  "Returns non-nil if the Emacs Lisp file FILE-NAME (or its
byte-compiled equivalent) has been loaded.

Specifically, the return value is the path of the file that was
loaded."
  (setq file-name (file-truename file-name))
  (and file-name
       (let* ((compiled-name (byte-compile-dest-file file-name))
              (names (list file-name compiled-name)))
         (some (lambda (entry)
                 (car (member (car entry) names)))
               load-history))))

(defun dependant-features (feature)
  "Returns a list of features that directly depend on FEATURE."
  (let ((dependency-map (make-hash-table :test 'eq))
        (missing (list)))
    (dolist (entry load-history)
      (let (provided-symbols required-symbols)
        (dolist (item (cdr entry))
          (case (car-safe item)
            (require (push (cdr item) required-symbols))
            (provide (push (cdr item) provided-symbols))))
        (dolist (required required-symbols)
          (when (eq t (gethash required dependency-map t))
            (puthash required provided-symbols dependency-map)))))
    ;; (maphash (lambda (k v)
    ;;            (message "%S => %S" k v))
    ;;          dependency-map)
    (gethash feature dependency-map)))

(provide 'my-load-path)

;; Local Variables:
;; no-byte-compile: t
;; End:
