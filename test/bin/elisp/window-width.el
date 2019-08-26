(defvar window-width-apply-timer nil)

;; (defcustom window-width-min-width 80
;;   "The minimum window width that `window-width-mode' attempts to
;; enforce."
;;   :group 'window-width)

(defun window-width--min-width (window)
  (with-current-buffer (window-buffer window)
    fill-column))

(defun window-width-window-configuration-change-hook ()
  (when window-width-apply-timer
    (cancel-timer window-width-apply-timer)
    (setq window-width-apply-timer nil))
  (when window-width-mode
    (setq window-width-apply-timer
          (run-with-timer 0.5 nil 'window-width-apply))))

(defun window-decoration-width (&optional window)
  (- (window-total-width window)
     (window-body-width window)))

(defun window-width-total-width-needed (window-tree)
  (if (windowp window-tree)
      (let* ((window window-tree)
             (frame (window-frame window))
             (total-width (window-pixel-width window))
             (body-width (window-body-width window t))
             (decoration-width (- total-width body-width)))
        (+ decoration-width
           (* (frame-char-width frame) (window-width--min-width window))))
    (let* ((vertical (car window-tree))
           (children (cddr window-tree))
           (sizes (mapcar 'window-width-total-width-needed children)))
      (if vertical
          (apply 'max sizes)
        (apply '+ sizes)))))
;;                20                  40                                      80                                     120
;;-----------------*-------------------*---------------------------------------*---------------------------------------*

(defun window-width-apply ()
  (let ((frame (selected-frame)))
    ;; Make sure the frame is wide enough to hold all the windows.
    (set-frame-width
     frame
     (max
      (frame-width frame)
      (/ (window-width-total-width-needed (car (window-tree)))
         (frame-char-width frame))))
    ;; Resize the windows to the correct size.
    (dolist (window (window-list  nil 'no-minibuffer))
      (let* ((target-width (window-width--min-width window))
             (body-width (window-body-width window))
             (delta (- target-width body-width))
             (horizontal t))
        (when (> delta 0)
          (when (window-resizable window delta horizontal)
            (window-resize window delta horizontal)))))

    (redraw-frame)))

(define-minor-mode window-width-mode
  "Tries to ensure that each window maintains a minimum width."
  :global t
  (add-hook 'window-configuration-change-hook
            'window-width-window-configuration-change-hook))

(provide 'window-width)
