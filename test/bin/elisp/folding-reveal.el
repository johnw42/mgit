;; Adds support to automaticall open and close folds in folding-mode.

(require 'folding)

(defvar folding-reveal-idle-timer nil)

(defconst folding-reveal-idle-timer-delay 1)

;; Flag used to prevent a fold from being auto-shown immediately after
;; it is hidden
(make-variable-buffer-local
 (defvar folding-reveal-just-hidden nil))

;; The location of the last automatically-shown fold.
(make-variable-buffer-local
 (defvar folding-reveal-last-location nil))

(defun folding-reveal-in-fold-p ()
  "Tests whether the current line is a folded section."
  (not (equal
        (save-excursion
          (end-of-line)
          (point))
        (save-excursion
          (folding-end-of-line)
          (point)))))

(defun folding-reveal-do-magic ()
  (if folding-reveal-just-hidden
        (setq folding-reveal-just-hidden nil)
      (when (and folding-mode
                 (folding-reveal-in-fold-p))
        (when folding-reveal-last-location
          (save-excursion
            (goto-char folding-reveal-last-location)
            (folding-hide-current-subtree))
          '(save-excursion (folding-whole-buffer)))
        (setq folding-reveal-last-location (point-marker))
        (while (folding-reveal-in-fold-p)
          (save-excursion
            (folding-show-current-entry))))))

(defun folding-reveal-idle-proc ()
  (when folding-mode
    (folding-reveal-do-magic)))

(defun folding-reveal-disable ()
  "Disable automatic revealing for folding-mode."
  (when folding-reveal-idle-timer
    (cancel-timer folding-reveal-idle-timer)
    (setq folding-reveal-idle-timer nil)))

(defun folding-reveal-set-just-hidden (&rest _)
  (setq folding-reveal-just-hidden t))

(defun folding-reveal-enable ()
  "Enable automatic revealing for folding-mode."

  (advice-add 'folding-hide-current-entry
              :after 'folding-reveal-set-just-hidden)
  (advice-add 'folding-hide-current-subtree
              :after 'folding-reveal-set-just-hidden)
  (advice-add 'folding-whole-buffer
              :after 'folding-reveal-set-just-hidden)

  ;; Schedule an idle timer to reveal folds.
  (unless folding-reveal-idle-timer
    (setq folding-reveal-idle-timer
          (run-with-idle-timer folding-reveal-idle-timer-delay
                               t
                               'folding-reveal-idle-proc))))

(add-hook 'folding-mode-hook 'folding-reveal-enable)
;;(remove-hook 'change-major-mode-hook 'folding-reveal-disable)

(provide 'folding-reveal)
