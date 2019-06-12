(defvar auto-minor-mode-alist nil
  "Similar to auto-mode-alist, but for minor modes.  When a
file's mode is set, every matching entry in this list is
processed.  Entries can take two forms:

  (REGEX MODE ARG)

    When the file's path matches REGEX, the function MODE is
    called with the argument ARG.

  (REGEX . MODE)

    Same as (REGEX MODE nil).
")

(defun auto-minor-mode-normal-mode-after (&rest args)
  (dolist (pair auto-minor-mode-alist)
    (let ((path (buffer-file-name))
          (pattern (car pair))
          (func-or-list (cdr pair)))
      (when (string-match pattern path)
        (if (consp func-or-list)
            (let ((func (car func-or-list))
                  (arg (cadr func-or-list)))
              (funcall func arg))
          (funcall func-or-list))))))

(advice-add 'normal-mode :after 'auto-minor-mode-normal-mode-after)

(provide 'auto-minor-mode)
