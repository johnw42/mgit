;; -*- lexical-binding: t -*-
(require 'reveal)

(defconst aggressive-reveal-watch-commands
  '(;;(next-line . overlay-start)
    ;;(previous-line . overlay-end)
    (right-char)
    (left-char)
    (forward-char)
    (backward-char))
  "An alist of commands the trigger an automatic reveal.  The `cdr' controls where the point is placed after an automatic reveal:

  nil - Don't move the point.
  overlay-start - At the start of the revealed text.
  overlay-end - At the end of the revealed text
")

(defun aggressive-reveal--ellipsis-p (overlay)
  "Tests wher an overlay acts as an ellipsis."
  ;; Based on code in reveal-open-new-overlays
  (and (overlay-start overlay)
       (let ((inv (overlay-get overlay 'invisible)))
         (and (consp buffer-invisibility-spec)
              (cdr-safe (assq inv buffer-invisibility-spec))))))

(defun aggressive-reveal--post-command ()
  ;; Look up the commend being executed.
  (let ((watch-command (assq this-command aggressive-reveal-watch-commands)))
    ;; Is it one of the commands that triggers an aggressive reveal?
    (when watch-command
      ;; Loop for overlays starting or ending at this position.
      (let* ((at-bol (overlays-at (point-at-bol)))
             (at-eol (overlays-at (point-at-eol)))
             (overlays (append at-bol at-eol)))
        (cl-block loop
          (dolist (overlay overlays)
            ;; Is this overlay an ellipsis?
            (when (aggressive-reveal--ellipsis-p overlay)
              ;; Decide what to do.
              (cl-case (cdr watch-command)
                (overlay-start
                 ;; Move to the start of the hidden text and let
                 ;; reveal-mode operate normally.
                 (goto-char (overlay-start overlay)))
                (overlay-end
                 ;; Move to the end of the hidden text and let
                 ;; reveal-mode operate normally.
                 (goto-char (1- (overlay-end overlay))))
                (t
                 ;; Move to a position that will trigger reveal-mode
                 ;; and force it to reveal the hidden text before
                 ;; moving the point back where it was.
                 (save-excursion
                   (goto-char (overlay-start overlay))
                   (reveal-post-command))))
              ;; No need to act on more than one overlay.
              (return-from loop))))))))

(defun aggressive-reveal--activate (active local)
  "Enable or disable aggressive-reval mode either globally or
locally."
  (if local
      (reveal-mode active)
    (global-reveal-mode active))
  (if active
      (add-hook 'post-command-hook
                'aggressive-reveal--post-command
                nil local)
    (remove-hook 'post-command-hook
                 'aggressive-reveal--post-command
                 local)))

(define-minor-mode aggressive-reveal-mode
  "Version of `reveal-mode' that expands when certain motion
commands are executed in a line that (visually) contains elided
text."
  :group 'reveal
  (aggressive-reveal--activate aggressive-reveal-mode t))

(define-minor-mode global-aggressive-reveal-mode
  "Global version of `aggressive-reveal-mode'."
  :group 'reveal
  (aggressive-reveal--activate global-aggressive-reveal-mode nil))

(provide 'aggressive-reval)
