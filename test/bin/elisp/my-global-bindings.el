;; -*- no-byte-compile: t -*-
(defvar original-global-keymap (copy-keymap (current-global-map))
  "A copy of default global keymap.")

(defun restore-global-binding (command)
  "Restore the key global binding for COMMAND the Emacs default."
  (interactive "C")
  (substitute-key-definition command command
                             (current-global-map)
                             original-global-keymap))

(provide 'my-global-bindings)

;; Local Variables:
;; no-byte-compile: t
;; End:
