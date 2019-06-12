;; -*- no-byte-compile: t -*-
;; ;; -*- mode: emacs-lisp; -*-

;; ;; TODO:
;; ;;   Enable text mode and line wrapping in files named /tmp/tmp*-review-email.

;; (require 'cl)

;; ;;;
;; ;;; Set up location for options specified through the customization
;; ;;; interface.
;; ;;;
;; (load-file custom-file)

;; (defvar deferred-init-hook nil)

;; ;;;
;; ;;; Specify locations where elisp code is found.
;; ;;;
;; (add-to-list 'load-path my-elisp-dir 'append)
;; (add-to-list 'load-path (expand-file-name "3rdparty/" my-elisp-dir) 'append)

;; ;;;
;; ;;; Global key findings.
;; ;;;
;; ;; (global-set-key [f7] 'my-save-and-compile)
;; (global-set-key (kbd "<f7>") 'my-google-save-and-build)
;; (global-set-key (kbd "<f8>") 'my-google-save-and-test)
;; (global-set-key (kbd "<f9>") 'my-google-save-and-run)
;; (global-set-key (kbd "C-z") nil)
;; (global-set-key (kbd "C-<return> b") 'my-google-save-and-build)
;; (global-set-key (kbd "C-<return> t") 'my-google-save-and-test)
;; (global-set-key (kbd "C-<return> r") 'my-google-save-and-run)
;; (global-set-key (kbd "s-<f7>") 'my-google-save-and-build)
;; (global-set-key (kbd "s-<f8>") 'my-google-save-and-test)
;; (global-set-key (kbd "s-<f9>") 'my-google-save-and-run)
;; (global-set-key (kbd "C-x C-b") 'bs-show)    ; replace list-buffers
;; ;; (global-set-key (kbd "C-z") 'keyboard-quit)  ; don't minimize the window!
;; (global-set-key (kbd "<f12>") 'my-redisplay)

;; (defun my-redisplay ()
;;   (interactive)
;;   ;;(message "my-redisplay")
;;   (make-frame-visible)
;;   ;;(raise-frame)
;;   ;;(redisplay t)
;;   ;;(force-window-update)
;;   )

;; ;;;
;; ;;; Short aliases for interactive commands.
;; ;;;
;; (defalias 'rev 'revert-buffer)
;; (defalias 'mv 'my-move-file)
;; (defalias 'cp 'my-copy-file)
;; (defalias 'rm 'my-delete-file)
;; (defalias '+x 'my-make-executable)

;; ;;;
;; ;;; Set up automatic modes.
;; ;;;
;; (add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
;; (add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))
;; (add-to-list 'auto-mode-alist '("crontab"    . crontab-mode))
;; (add-to-list 'auto-mode-alist '("/\\.runlog\\.d/log$" . auto-revert-mode))
;; (add-to-list 'auto-mode-alist '("\\.yaml$"    . yaml-mode))

;; (case emacs-major-version
;;   (24
;;    (setq find-function-C-source-directory
;;          (expand-file-name "~/src/emacs-24.2/src")))
;;   (23
;;    (setq find-function-C-source-directory
;;          (expand-file-name "~/src/emacs23-23.1+1/src"))))


;; (setq folding-mode-prefix-key (kbd "C-c"))

;; ;;;
;; ;;; Deferred initialization.
;; ;;;

;; (defun try-require (sym &optional precondition)
;;   "Tries to require a symbol.  Prints a warning message but won't
;; generate an error if the symbol can't be found or if there is an
;; error loading it.."
;;   (if debug-on-error
;;       (require sym)
;;     (condition-case nil
;;         (progn (require sym) t)
;;       (error
;;        (message "Unable to require symbol: %S" sym)
;;        nil))))

;; (try-require 'load-path-alias)
;; (try-require 'crontab-mode)
;; (try-require 'paredit)
;; (try-require 'auto-eval-buffer)
;; (try-require 'my-edit-variable)
;; (try-require 'my-files)
;; (try-require 'my-lisp 'lisp-mode)
;; (try-require 'my-move-region-to-file)
;; (try-require 'my-shell 'shell)
;; (try-require 'my-java)
;; (try-require 'my-js2)
;; (when t ;; (= 23 emacs-major-version) ;; XXX
;;   (try-require 'my-google)
;;   (try-require 'my-google-java)
;;   (try-require 'my-google-jstd-tests)
;;   (try-require 'google-buildifier))
;; (try-require 'my-python)
;; (try-require 'eless)
;; (try-require 'conflicts)
;; (try-require 'my-mouse-drag-region)
;; (try-require 'my-misc)
;; (try-require 'my-compile)
;; (try-require 'unit-test-utils)
;; (try-require 'my-hooks)
;; (try-require 'yaml-mode)

;; (try-require 'flymake)
;; (try-require 'csharp-mode)

;; ;; (when (try-require 'pymacs)
;; ;;   ;; See http://pymacs.progiciels-bpi.ca/pymacs.html
;; ;;   (add-to-list 'pymacs-load-path my-elisp-dir)
;; ;;   (pymacs-load "pymacs_test"))

;; ;; (require 'auto-fold)
;; ;; (auto-fold-mode 1)

;; (defmacro deftimer (timer-name timer-expr)
;;   `(progn
;;      (defvar ,timer-name nil)
;;      (when ,timer-name
;;        (cancel-timer ,timer-name)
;;        (setq ,timer-name nil)
;;        (setq ,timer-name ,timer-expr))))

;; (add-hook
;;  'auto-save-hook
;;  (defun my-desktop-save-auto-save-hook ()
;;    (when desktop-save-mode
;;      (deftimer my-desktop-save-timer
;;        (run-with-idle-timer
;;         60 nil
;;         (lambda ()
;;           (with-temp-message "Saving desktop..."
;;             (desktop-save-in-desktop-dir))))))))

;; (defun my-new-script-file ()
;;   (insert "#! /bin/bash\n")
;;   (set-buffer-modified-p nil)
;;   (my-make-executable))

;; ;; (defun wrap-nonlist-in-list (value)
;; ;;   (if (listp value)
;; ;;       value
;; ;;     (list value)))

;; ;; (defun local-and-global-values (var)
;; ;;   "Concatenate the buffer-local and global values of VAR."
;; ;;   (append
;; ;;    (and (local-variable-p var)
;; ;;         (buffer-local-value var))
;; ;;    (default-value var)))

;; ;; (defvar 'my-alternate-file-path-functions nil)
;; ;; (add-hook 'my-alternate-file-path-functions
;; ;;           (defun my-guess-alternate-file-paths (path)))

;; ;; ;; Run the deferred initialization after the UI has had a chance to
;; ;; ;; appear.  This way the user can see progress messages as various
;; ;; ;; things are loaded.
;; ;; (if after-init-time
;; ;;     (run-hooks 'deferred-init-hook)
;; ;;   (run-with-idle-timer
;; ;;    0 nil
;; ;;    (lambda ()
;; ;;      (with-demoted-errors
;; ;;        (run-hooks 'deferred-init-hook))
;; ;;      (when (daemonp)
;; ;;        (daemon-init)))))

;; (defun set-lisp-indent (symbol indent-level)
;;   (put symbol 'lisp-indent-function indent-level))
;; (set-lisp-indent 'unit-test 1)
;; (set-lisp-indent 'test-group 1)
;; (set-lisp-indent 'unit-test-suite 1)

;; ;; (add-to-list 'load-path
;; ;;              "/home/jrw/git5/exp/google3/experimental/users/jrw/pymacs")
;; ;; (require 'pymacs)
;; ;; (setq pymacs-binary "/home/jrw/git5/exp/google3/blaze-bin/experimental/users/jrw/pymacs/pymacs_server")
;; ;; (pymacs-extend-path (concat user-emacs-directory "python"))
;; ;; (pymacs-start)
;; ;; (message "python path: %s" (pymacs-repr (pymacs-get nil "sys.path")))

;; (defun gnome-save-yourself ()
;;   (desktop-save "~")

;;   (run-with-idle-timer 0 nil 'kill-emacs))


;; (define-abbrev
;;   global-abbrev-table
;;   "todo"
;;   "TODO(jrw)"
;;   nil
;;   :case-fixed t
;;   :system t)

;; (define-abbrev
;;   global-abbrev-table
;;   "TODO"
;;   "TODO(jrw)"
;;   nil
;;   :case-fixed t
;;   :system t)


;; ;; Emacs24-specific stuff.
;; (condition-case err
;;     (progn
;;       ;; (electric-indent-mode)
;;       ;; (electric-layout-mode)
;;       )
;;   (error nil))



;; (defvar my-emacs-instance nil)

;; (defun my-emacs-instance-init (option-name)
;;   (message "my-emacs-instance-init")
;;   (setq my-emacs-instance (intern (pop command-line-args-left)))
;;   (when my-emacs-instance
;;     (unless (eq 'default my-emacs-instance)
;;       (setq frame-title-format
;;             (let ((instance-str (format "[%s]" my-emacs-instance)))
;;               `(multiple-frames
;;                 (,instance-str "%b")
;;                 (,instance-str invocation-name "@" system-name))))
;;       (set-face-attribute 'mode-line nil
;;                           :background "yellow")
;;       (setq server-name (symbol-name my-emacs-instance))
;;       (setq savehist-file
;;             (expand-file-name
;;              (format "history.%s" my-emacs-instance)
;;              user-emacs-directory))
;;       (setq desktop-base-file-name
;;             (format ".%s.emacs.desktop" my-emacs-instance)))
;;     (server-mode 1)
;;     (desktop-read "~")
;;     (desktop-save-mode 1)
;;     (savehist-mode 1)
;;     (defvar my-desktop-save-timer
;;       (run-with-idle-timer 60 t 'desktop-save "~"))
;;     (setq inhibit-splash-screen t)))

;; (add-to-list
;;  'command-switch-alist
;;  '("-instance" . my-emacs-instance-init))



;; (global-set-key (kbd "S-C-x S-C-f") 'my-find-file-from-clipboard)
;; (defun my-find-file-from-clipboard ()
;;   (interactive)
;;   (find-file (x-get-selection)))

;; (add-hook 'window-scroll-functions
;;           (defun my-window-scroll-function (window position)
;;             (run-with-idle-timer 0.1 nil 'make-frame-visible
;;                                  (window-frame window))))

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (package-initialize)

;; Local Variables:
;; no-byte-compile: t
;; End:
