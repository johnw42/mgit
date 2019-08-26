;; -*- no-byte-compile: t -*-
(require 'my-files)
(require 'my-python)


(add-hook
 'find-file-not-found-functions
 (defun my-find-file-not-found-function ()
   (let ((path (buffer-file-name))
         alt-name)
     (save-match-data
       (cond

        ;; Handle cases when a file name has a line and column number appended.
        ((and (string-match "\\(.*\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?"
                            path)
              (setq alt-name
                    (match-string 1 path))
              (file-exists-p alt-name))
         (find-alternate-file alt-name)
         (forward-line (max 0 (1- (string-to-number (match-string 2 path)))))
         (forward-char (max 0 (string-to-number (match-string 4 path))))
         t)

        ((equal (file-truename (file-name-directory path))
                (file-truename (expand-file-name "~/bin/")))
         (my-new-script-file)
         t))))))

(defun my-new-script-file ()
  (message "my-new-script-file")
  (let* ((is-wrapper
          (loop for path-elem in (string-split ":" (getenv "PATH"))
                with basename = (file-name-nondirectory (buffer-file-name))
                for program = (concat
                               (file-name-as-directory path-elem)
                               basename)
                if (file-executable-p program) return t))
         (template (concat "
#! /usr/bin/python
import argparse
import logging
import os
import re
import subprocess
import sys" (and is-wrapper "

from jrw import wrapper") "


class App(object):

  def Run(self, args):
    <CURSOR>" (and is-wrapper "
    wrapper.ExecWrappedProgram()") "


def main():
  parser = argparse.ArgumentParser(description='')
  parser.add_argument('--debug', action='store_true')
  args = parser.parse_args()
  if args.debug:
    logging.basicConfig(level=logging.DEBUG)
  App().Run(args)


if __name__ == '__main__': main()")))
    (mypy-insert-file-template template)
    (set-buffer-modified-p nil)
    (my-make-executable)))

(provide 'my-file-not-found)

;; Local Variables:
;; no-byte-compile: t
;; End:
