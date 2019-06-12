;; -*- no-byte-compile: t -*-
;; Copyright 2012 Google Inc. All Rights Reserved.
;; Author: jrw@google.com (John Williams)


(defun move-region-to-file (start end filename)
  (interactive
   (list (region-beginning)
         (region-end)
         (read-file-name "File to move to: ")))
  (kill-region start end)
  (find-file filename)
  (end-of-buffer)
  (undo-boundary)
  (yank))

(provide 'my-move-region-to-file)

;; Local Variables:
;; no-byte-compile: t
;; End:
