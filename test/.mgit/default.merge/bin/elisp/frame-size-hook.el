(require 'tmessage)

(tmessage "" "loading frame-size-hook")

(defun frame-size-window-configuration-change-hook ()
  (when frame-size-hook-apply-timer
    (cancel-timer frame-size-hook-apply-timer)
    (setq frame-size-hook-apply-timer nil))
  (setq frame-size-hook-apply-timer
        (run-with-timer 0.5 nil 'frame-size-hook-apply)))

(defvar frame-size-hook-apply-timer nil)

(defconst frame-size-hook-min-width 80)

(defun window-decoration-width (&optional window)
  (- (window-total-width window)
     (window-body-width window)))

(defun frame-size-hook-total-width-needed (window-tree)
  (if (windowp window-tree)
      (let* ((window window-tree)
             (frame (window-frame window))
             (total-width (window-pixel-width window))
             (body-width (window-body-width window t))
             (decoration-width (- total-width body-width)))
        (+ decoration-width
           (* (frame-char-width frame) frame-size-hook-min-width))
        ;;(+ decoration-width frame-size-hook-min-width)
        )
    (let* ((vertical (car window-tree))
           (children (cddr window-tree))
           (sizes (mapcar 'frame-size-hook-total-width-needed children)))
      (if vertical
          (apply 'max sizes)
        (apply '+ sizes)))))
;;                20                  40                                      80                                     120
;;-----------------*-------------------*---------------------------------------*---------------------------------------*

(defun frame-size-hook-apply ()
  (let ((frame (selected-frame)))
    ;; Make sure the frame is wide enough to hold all the windows.
    (set-frame-width
     frame
     (max
      (frame-width frame)
      (/ (frame-size-hook-total-width-needed (car (window-tree)))
         (frame-char-width frame))))
    ;; Resize the windows to the correct size.
    (dolist (window (window-list  nil 'no-minibuffer))
      (let* ((target-width frame-size-hook-min-width)
             (body-width (window-body-width window))
             (delta (- target-width body-width))
             (horizontal t))
        (when (> delta 0)
          (when (window-resizable window delta horizontal)
            (window-resize window delta horizontal)))))

    (redraw-frame)))

(defun frame-size-hook-install ()
  (add-hook 'window-configuration-change-hook
            'frame-size-window-configuration-change-hook))

(provide 'frame-size-hook)
