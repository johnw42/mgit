;; TODO:
;; - find and remove uses of add-major-mode-hook and add-file-hook
;; - get rid of my-hooks.el


;; (require 'cl-macs)
;; (require 'cl-lib)
;; (require 'cl)

(defun load-init-file (path)
  (with-demoted-errors (format "Error in %s: %%s" path)
    (load (expand-file-name path))))

(load-init-file "~/.emacs.d/init-compat.el")
(load-init-file "~/.emacs.d/init-el-get.el")
(load-init-file "~/.emacs.d/init-use-package.el")
(load-init-file "~/.emacs.d/init-load-path.el")
(load-init-file "~/.emacs.d/init-byte-compile.el")
(load-init-file "~/.emacs.d/init-helm.el")
(load-init-file "~/.emacs.d/init-paredit.el")
(load-init-file "~/.emacs.d/init-ui-tweaks.el")
(load-init-file "~/.emacs.d/init-global-bindings.el")
(load-init-file "~/.emacs.d/init-frame.el")
(load-init-file "~/.emacs.d/init-grep.el")
(load-init-file "~/.emacs.d/init-c-source.el")
(load-init-file "~/.emacs.d/init-python.el")
(load-init-file "~/.emacs.d/init-js3-mode.el")
(load-init-file "~/.emacs.d/init-haskell.el")
(load-init-file "~/.emacs.d/init-minor-modes.el")
(load-init-file "~/.emacs.d/init-packages.el")
(load-init-file "~/.emacs.d/init-chromium.el")
;; (load-init-file "~/.emacs.d/init-command-frequency.el")
;; (load-init-file "~/.emacs.d/init-fold-dwim.el")
;; (load-init-file "~/.emacs.d/init-my-window.el")

(eval-after-load 'server
  '(progn
     (load-init-file "~/.emacs.d/init-solarized.el")
     (load-init-file "~/.emacs.d/init-google.el")))

;; Load desktop last since it may depend on things loaded earlier
;; (e.g. js3-mode).
(load-init-file "~/.emacs.d/init-desktop.el")

(defun confirm-kill-emacs-predicate (prompt)
  (or (not server-mode)
      (y-or-n-p prompt)))
(setq confirm-kill-emacs 'confirm-kill-emacs-predicate)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list
   (quote
    ("/google/src/head/depot/google3/third_party/emacs/info/")))
 '(abbrev-mode t t)
 '(ansi-color-names-vector
   ["black" "red2" "green3" "gold3" "blue" "magenta3" "cyan3" "white"])
 '(auto-hscroll-mode t)
 '(auto-revert-interval 2)
 '(auto-save-file-name-transforms
   (quote
    ((".*/\\([^/]+\\)" "/tmp/\\2" t)
     ("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t))))
 '(auto-window-vscroll t t)
 '(backup-directory-alist (quote (("." . "~/backups/emacs"))))
 '(before-save-hook
   (quote
    (my-hooks-before-save-hook delete-trailing-whitespace)))
 '(blink-matching-delay 0.25)
 '(case-fold-search t)
 '(clean-buffer-list-delay-general 7)
 '(column-number-mode t)
 '(command-frequency-mode t)
 '(comment-style (quote indent))
 '(compilation-context-lines 0)
 '(compilation-message-face (quote default))
 '(compilation-scroll-output t)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
 '(debug-on-quit nil)
 '(default-frame-alist (quote ((height . 75))))
 '(desktop-restore-frames nil)
 '(eval-expression-print-level nil)
 '(fci-rule-color "#073642")
 '(folding-allow-overlays nil)
 '(global-auto-revert-ignore-buffer nil t)
 '(google3-build-command "_blaze_build_wrapper")
 '(grep-command "zgrep -nHEi -e ")
 '(grep-find-template
   "find . <X> -type f <F> -print0 | xargs -0 -e zgrep -E <C> -nHi -e <R>")
 '(grep-template "zgrep -E <C> -nHi -e <R> <F>")
 '(haskell-indent-spaces 4)
 '(haskell-indentation-ifte-offset 2)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(haskell-indentation-show-indentations-after-eol nil)
 '(haskell-indentation-starter-offset 4)
 '(haskell-process-log t)
 '(haskell-process-suggest-overloaded-strings t)
 '(helm-buffer-details-flag nil)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hindent-style "johan-tibell")
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(ido-decorations
   (quote
    ("{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(inverse-video nil)
 '(ispell-extra-args (quote ("--run-together")))
 '(jde-jdk-registry (quote (("1.5" . "/usr/local/devh/packages/jdk"))))
 '(js-expr-indent-offset 2)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-global-externs
   (quote
    ("goog" "describe" "it" "expect" "beforeEach" "afterEach" "inject" "jasmine" "spyOn")))
 '(js2-indent-on-enter-key t)
 '(js2-mirror-mode nil)
 '(js2-mode-escape-quotes nil)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-continued-expr-mult 2)
 '(js3-expr-indent-offset 2)
 '(js3-global-externs
   (quote
    ("goog" "castv2" "cloudview" "chrome" "Promise" "console" "sinon" "QUnit" "base" "remoting")))
 '(js3-max-columns 80)
 '(js3-paren-indent-offset 2)
 '(js3-strict-missing-semi-warning t)
 '(kill-whole-line t)
 '(large-file-warning-threshold 104857600)
 '(load-home-init-file t t)
 '(magit-diff-use-overlays nil)
 '(message-log-max 5000)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode t nil (mwheel))
 '(next-error-recenter (quote (4)))
 '(nxml-auto-insert-xml-declaration-flag t)
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(org-hide-leading-stars t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(python-indent-offset 2)
 '(read-file-name-completion-ignore-case t)
 '(read-quoted-char-radix 16)
 '(revert-without-query (quote (".*")))
 '(rmail-summary-scroll-between-messages t)
 '(safe-local-variable-values
   (quote
    ((hindent-style . "chris-done")
     (whitespace-style face tabs trailing lines-tail)
     (eval hs-hide-all)
     (lexical-bindings . t)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (eval add-hook
           (quote after-save-hook)
           (quote byte-compile-from-hook)
           nil :local)
     (profile-emacs-init)
     (sh-indent-comment . t)
     (auto-recompile . t)
     (run-after-edit . true)
     (unit-tests-enabled . t)
     (py-indent-offset . 4)
     (sh-basic-offset . 2))))
 '(savehist-mode t)
 '(scroll-bar-mode (quote right))
 '(scroll-bar-width nil t)
 '(scroll-conservatively 1 nil nil "Setting this to 1 does a pretty good job of always scrolling by a single line, but recentering for commands that jump way outside the visible area.  It has the bad side-effect that jumping to a page break that is only a single line off the edge of the screen will not recenter.  Increasing the value exacerbates this problem.

This also has a problem when emacs can't render text fast enough to keep up; in that case, the point is always recentered.")
 '(send-mail-function nil)
 '(server-use-tcp nil)
 '(set-mark-command-repeat-pop t)
 '(sh-indentation 2)
 '(show-paren-mode t)
 '(slime-kill-without-query-p t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(split-width-threshold 160)
 '(standard-indent 2)
 '(tags-case-fold-search nil)
 '(tags-revert-without-query t)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t)
 '(vertical-scroll-bar t t)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(window-combination-resize t)
 '(window-width-mode t)
 '(x-select-enable-clipboard t)
 '(yas-choose-keys-first t)
 '(yas-good-grace nil))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(git-commit-nonempty-second-line-face ((t nil)))
 '(git-commit-overlong-summary-face ((t nil)))
 '(highlight-beyond-fill-column-face ((t (:foreground "red" :underline t :weight bold))))
 '(sh-heredoc ((t (:foreground "forest green")))))

;; Local Variables:
;; mode: emacs-lisp
;; End:
