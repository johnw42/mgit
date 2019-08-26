;; -*- no-byte-compile: t -*-
(require 'string-util)

(defun file-name-prefix-p (possible-prefix full-path)
  (or (equal possible-prefix full-path)
      (string-prefix-p (string-with-suffix "/" possible-prefix)
                       full-path)))

(defun file-name-suffix-p (possible-suffix full-path)
  (or (equal possible-suffix full-path)
      (string-suffix-p (string-with-suffix
                        "/" (concat "/" possible-suffix))
                       (string-with-suffix
                        "/" full-path))))

(provide 'my-file-names)

;; Local Variables:
;; no-byte-compile: t
;; End:
