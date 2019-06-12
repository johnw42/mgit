;; -*- lexical-binding: t -*-

(require 'compile)

(add-to-list 'compilation-finish-functions
             'notify-after-compile)

(defun notify-after-compile (buffer result)
  (start-process "notify-send" nil
                 "notify-send"
                 "Compilation Finished"
                 result))

(provide 'notify-after-compile)
