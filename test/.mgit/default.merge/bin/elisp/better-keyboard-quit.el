(defvar better-keyboard-quit--last-call 0.0)
(defvar better-keyboard-quit--interval 0.5)

(defun better-keyboard-quit ()
  "Use this function in place of `keyboard-quit' so that typing
C-g twice rapidly will abort any recursive minibuffer in
progress."
  (interactive)
  (let ((then better-keyboard-quit--last-call)
        (now (float-time)))
    (setq better-keyboard-quit--last-call now)
    (when (and (< (- now then) better-keyboard-quit--interval)
               (> (minibuffer-depth) 0))
      (abort-recursive-edit))
    (keyboard-quit)))

(provide 'better-keyboard-quit)
