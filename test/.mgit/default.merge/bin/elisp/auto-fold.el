;; -*- no-byte-compile: t -*-
;; Minor mode to automatically expand and collapse folding-mode folds.

(require 'my-util)

(defvar auto-fold-mode-timers nil)
(defconst auto-fold-mode-expand-timeout 0.25)
(defconst auto-fold-mode-collapse-timeout 1)
;; (defvar auto-unfolds nil)
(defvar auto-unfold-count)
(defvar auto-unfold-marker)
(defvar auto-unfold-needs-collapse)
(make-variable-buffer-local 'auto-unfolds)

(define-minor-mode auto-fold-mode
  "Mode to automatically expand and collapse folded code
  regions."
  :global t
  :lighter " AF"

  (mapc 'cancel-timer auto-fold-mode-timers)
  (setq auto-fold-mode-timers nil)
  (when auto-fold-mode
    (setq auto-unfold-count 0)
    (setq auto-unfold-marker nil)
    (setq auto-unfold-needs-collapse nil)
    (setq auto-fold-mode-timers
          (list
           (run-with-idle-timer
            auto-fold-mode-expand-timeout t
            'auto-fold-timer-func 'expand)
           (run-with-idle-timer
            auto-fold-mode-collapse-timeout t
            'auto-fold-timer-func 'collapse)))))

(defun auto-fold-timer-func (mode)
  (when (and (boundp 'folding-mode)
             folding-mode)
    (when (and (eq mode 'collapse)
               auto-unfold-needs-collapse)
      (auto-fold-collapse))
    (auto-fold-expand)))

(defun auto-fold-expand ()
  (let ((fold-marker (car (folding-get-mode-marks))))
    (while
        (save-excursion
          (beginning-of-line)
          (and (looking-at (concat "\\s-*" (regexp-quote fold-marker)))
               (eq 0 (folding-mark-look-at))))
      (auto-fold-do-expand))))

(defun auto-fold-do-expand ()
  (setq auto-unfold-needs-collapse t)
  (save-excursion
    (folding-show-current-entry)))

(defun auto-fold-collapse ()
  (setq auto-unfold-needs-collapse nil)
  (save-excursion
    (folding-whole-buffer)))

(provide 'auto-fold)

;; Local Variables:
;; no-byte-compile: t
;; End:
