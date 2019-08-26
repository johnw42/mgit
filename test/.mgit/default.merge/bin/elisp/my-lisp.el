;; -*- no-byte-compile: t -*-
;; Settings for Lisp modes.
(require 'my-hooks)
(require 'my-imports)
(require 'rx)
(require 'thingatpt)

;;;###autoload
(progn
  (add-major-mode-hook 'clojure-mode 'my-clojure-mode-hook)
  (add-major-mode-hook 'emacs-lisp-mode 'my-emacs-lisp-mode-hook)
  (add-major-mode-hook 'lisp-interaction-mode 'my-emacs-lisp-mode-hook)
  (add-major-mode-hook 'edebug-eval-mode 'my-edebug-eval-mode-hook))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "{") #'paredit-open-curly)
     (define-key paredit-mode-map (kbd "}") #'paredit-close-curly)))

(defun my-lisp-shared-mode-hook ()
  ;;(require 'autopair)
  ;;(autopair-mode)

(require 'my-interactive)
  ;;(require 'paredit)
  (paredit-mode 1)
  (local-set-key (kbd "RET") 'my-lisp-electric-ret))

(defun my-lisp-electric-ret (arg)
  (interactive "P")
  (require 'cl)
  ;; (dolist (close-char (string-to-list ")]}"))
  ;;   (when (eq (char-after) close-char)
  ;;     (save-excursion (newline-and-indent))))
  (newline arg)
  (indent-according-to-mode))

;;;
;;; Clojure
;;;

;;;###autoload
(defun my-clojure-mode-hook ()
  (my-lisp-shared-mode-hook)
  ;; (local-set-key (kbd "^") 'my-clojure-electric-caret)
  )

;; (defun my-clojure-electric-caret ()
;;   (interactive)
;;   (unless (looking-back "#")
;;     (insert "#"))
;;   (insert "^"))

(eval-after-load 'clojure-mode
  '(progn
     (modify-syntax-entry ?= "w" clojure-mode-syntax-table)
     
     (define-clojure-indent
       ;; clojure core
       (-> 0)
       (->> 0)
       (send 1)
       (def 1)
       (case 1)
       (into 1)
       (apply 1)

       ;; compojure
       (GET 1)
       (POST 1)
       (ANY 1))
     
     (defvar old-clojure-font-lock-keywords clojure-font-lock-keywords)
     (setq clojure-font-lock-keywords
           `(
             ;; Clojure 1.2 syntax for metadata.
             ("\\^\\sw+" 0 font-lock-type-face)
             (,(concat "(\\(?:clojure.core/\\)?\\(fn\\)[ \t]+"
                       ;; Possibly type
                       "\\(?:#?^\\sw+[ \t]*\\)?"
                       ;; Possibly name
                       "\\(\\sw+\\)?" )
              (1 font-lock-keyword-face)
              (2 font-lock-function-name-face nil t))
             (,(concat "(\\(?:clojure.core/\\)?\\("
                       (regexp-opt '("defn" "defn-" "def" "def-" "defonce"
                                     "defmulti" "defmethod" "defmacro"
                                     "defstruct" "deftype" "defprotocol"
                                     "defrecord" "defvar" "defunbound"
                                     "defalias" "defhinted"
                                     "defnk" "defn-memo"))
                       ;; Function declarations.
                       "\\)\\>"
                       ;; Any whitespace
                       "[ \r\n\t]*"
                       ;; Possibly type or metadata
                       "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)?"

                       "\\(\\sw+\\)?")
              (1 font-lock-keyword-face)
              (2 font-lock-function-name-face nil t))
              
             ;; Function-arguments.
             ;; TODO: handle multi-arity functions
             ;; ("(\\(defn\\|fn\\)\\>"
             ;;  (1 font-lock-keyword-face)
             ;;  (my-clojure-match-fn-arg
             ;;   (my-clojure-skip-to-fn-args) nil
             ;;   (0 font-lock-variable-name-face)))

             ;; (my-clojure-match-binding-form
             ;;  (1 font-lock-keyword-face)
             ;;  (my-clojure-match-queued-variable-name
             ;;   nil nil
             ;;   (0 font-lock-variable-name-face)))

             ;; "let" bindings
             ;; ("\\<let\\>" "\\<\\sw+\\>"
             ;;  (my-clojure-skip-to-fn-args) nil
             ;;  (0 font-lock-variable-name-face))
             
             ;; Keywords.
             ("\\<:\\(\\sw\\|#\\)+\\>" 0 font-lock-constant-face)

             ;; #() functions
             ;; ("\\(#\\)(" 1 font-lock-keyword-face)

             ;; Arguments to #() functions
             ("\\<%\\([0-9]*\\|&\\)\\>" 0 font-lock-variable-name-face)
             
             ;; Symbols suffixed with #
             ("\\<\\sw+\\(#\\)" 0 font-lock-variable-name-face)

             ;; ;; The & symbol.
             ;; ("\\<&\\>" 0 font-lock-constant-face)
             
             ;; Special characters.
             ("[~@]\\|~@" 0 font-lock-keyword-face)
      
             ;; Missing from core.
             (,(concat
                "(\\(?:clojure.core/\\)?"
                (regexp-opt
                 '("->>" "with-out-str" "case") t)
                "\\>")
              1 font-lock-keyword-face)
             (,(concat
                "(\\(?:clojure.core/\\)?"
                (regexp-opt
                 '("assert" "assoc-in" "get" "get-in" "keyword" "list" "list*"
                   "name" "not=" "seq" "select-keys" "set!" "update-in") t)
                "\\>")
              1 font-lock-builtin-face)
             
             ,@old-clojure-font-lock-keywords
             ))))

(defvar my-clojure-match-queue nil)

(defun my-clojure-match-bound-names ()
  "Match symbols in the current sexp and push their locations
onto *match-stack*."
  (forward-sexp)
  (let ((sexp-end (point)))
    (backward-sexp)
    (when (looking-at "#?^")
      ;; Skip over metadata.
      (goto-char (match-end 0))
      (forward-sexp 2)
      (setq sexp-end (point))
      (backward-sexp))
    (while (re-search-forward "\\(#?^\\|\\<:\\|\\<&\\>\\)\\|\\(\\<\\sw+\\>\\)" sexp-end t)
      (cond
       ((match-beginning 1)
        (goto-char (match-beginning 1))
        (forward-sexp))
       (:else
        (push (list (match-beginning 2)
                    (match-end 2))
              *match-stack*))))
    (goto-char sexp-end)))

(defun my-skip-to-sexp-start ()
  "Skip to the start of the next sexp."
  (condition-case err
      (progn
        (forward-sexp)
        (backward-sexp))
    (scan-error nil)))

(defun my-clojure-match-binding-form (limit)
  (let (*match-stack*)
    (condition-case err
        (when (re-search-forward
               (rx "("
                   (or (group (or "defn" "defn-" "defmacro" "fn"))
                       (group (or "let" "if-let" "when-let" "loop"
                                  "with-open" "for" "doseq")))
                   symbol-end)
               limit t)
          (save-match-data
            (save-excursion
              (goto-char (1+ (match-beginning 0)))
              (put-text-property (point)
                                 (save-excursion (forward-sexp) (point))
                                 'font-lock-multiline t))
            (cond
             ((match-beginning 1)
              ;; Find function args.
              (save-excursion
                (up-list)
                (forward-sexp)
                (setq multiline-end (point)))
              (while (and (<= (point) multiline-end)
                          (not (looking-back "]")))
                (forward-sexp))
              (backward-sexp)
              (my-clojure-match-bound-names))
             ((match-beginning 2)
              ;; Find bound variables.
              (forward-sexp)
              (when (looking-back "]")
                (let ((bindings-end (point)))
                  (backward-sexp)
                  (down-list)
                  (while (<= (point) bindings-end)
                    (my-clojure-match-bound-names)
                    (forward-sexp))))))))
      (scan-error nil))
    (when (match-end 0)
      (set-match-data (list (match-beginning 0)
                            (match-end 0)
                            (1+ (match-beginning 0))
                            (match-end 0)))
      (goto-char (match-end 0))
      (setq my-clojure-match-queue (nreverse *match-stack*)))))

(defun my-clojure-match-queued-variable-name (limit)
  (when my-clojure-match-queue
    (set-match-data (pop my-clojure-match-queue))
    t))

(defun my-clojure-match-queue-limit ()
  (when my-clojure-match-queue
    (cadr (car (last my-clojure-match-queue)))))

(defun my-clojure-skip-to-fn-args ()
  (let (fn-start fn-end)
    (save-excursion
      (up-list)
      (setq fn-start (point))
      (forward-sexp)
      (setq fn-end (point)))
    (while (and (<= (point) fn-end)
                (not (looking-back "]")))
      (forward-sexp))
    (let ((limit (point)))
      (backward-sexp)
      (down-list)
      (put-text-property fn-start fn-end 'font-lock-multiline t)
      limit)))

(defun my-clojure-match-fn-arg (limit)
  (catch 'found
    (while (and (re-search-forward "\\<\\sw+\\>" limit t)
                (save-match-data
                  (save-excursion
                    (goto-char (match-beginning 0))
                    (or (looking-at "[:0-9]\\|&\\>")
                        (throw 'found t))))))))

;; (defun clojure-indent-hof (state indent-point)
;;   (save-excursion
;;     (goto-char indent-point)))

;;;
;;; Emacs Lisp
;;;

;;;###autoload
(defun my-emacs-lisp-mode-hook ()
  (eldoc-mode 1)
  (setq dabbrev-search-function 'my-lisp-dabbrev-search)
  (local-set-key (kbd "C-m") 'newline)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "M-.") 'my-elisp-find-tag)
  (local-set-key (kbd "C-c i") 'my-imports-interactive-import)

  (my-lisp-shared-mode-hook)

  (setq my-imports-default-input-function
        'my-elisp-default-import-input)
  (setq my-imports-import-completions-function
        (lambda () features))
  (setq my-imports-parse-import-line-function
        'my-elisp-parse-import-line)
  (setq my-imports-make-import-line-function
        'my-elisp-make-import-line))

(defun my-edebug-eval-mode-hook ()
  (paredit-mode 0))

(defun my-elisp-default-import-input ()
  (let* ((symbol-name (thing-at-point 'symbol))
         (symbol (intern-soft symbol-name))
         (feature (and symbol
                       (my-elisp-symbol-to-feature symbol))))
    (and feature
         (symbol-name feature))))

(defun my-elisp-symbol-to-feature (symbol)
  (assert (symbolp symbol))
  (if (featurep symbol)
      symbol
    (catch 'found
      (dolist (pair load-history)
        (let (provided-name has-match)
          (dolist (entry (cdr pair))
            (if (eq symbol entry)
                (setq has-match t)
              (case (car-safe entry)
                (provide (setq provided-name (cdr-safe entry)))
                ((nil require))
                (t (when (eq symbol (cdr-safe entry))
                     (setq has-match t)))))
            (when (and has-match provided-name)
              (throw 'found provided-name))))))))

(defun my-elisp-make-import-line (import)
  (concat "(require '" (my-imports-parsed-package import) ")"))

(defun my-elisp-parse-import-line (line)
  (let ((case-fold-search nil))
    (save-match-data
      (when (string-match (rx "(require"
                              (* " ")
                              "'"
                              (group
                               (+ (or (syntax word)
                                      (syntax symbol))))
                              (* " ")
                              ")")
                          line)
        (make-my-imports-parsed :package (match-string 1 line))))))

(defun my-elisp-find-tag (symbol)
  ;;(interactive (find-function-read))
  ;;(interactive "SFunction: ")
  (interactive (list (my-read-function-or-variable nil nil nil)))
  (ring-insert find-tag-marker-ring (point-marker))
  (if (boundp symbol)
      (find-variable symbol)
    (find-function symbol)))

(defun my-lisp-dabbrev-search (abbrev reverse)
  "Search for an expansion of a Lisp symbol.  Expansions my have
an arbitrary prefix added as long as the prefix ends with '-',
'--', or ':'.

The names 'foo', and 'foo-bar', but not just 'bar', can expand to
these options:

  myprefix-foo-bar
  my-prefix:foo-bar
  my-prefix--foo-bar
"
  (let ((pattern (concat "\\(\\sw+-\\|\\(\\sw\\|-\\)+\\(--\\|:\\)\\)"
                         (regexp-quote abbrev)
                         "\\(\\sw\\|\\s_\\)*"
                         "\\|"
                         (regexp-quote abbrev)
                         "\\(\\sw\\|\\s_\\)+"))
        found-string)

    (while (and (not found-string)
                (if reverse
                    (re-search-backward pattern nil t)
                  (re-search-forward pattern nil t)))
      (goto-char (match-beginning 0))
      (if (bobp) nil
        (save-match-data
          (while (and (not (bobp))
                      (progn (backward-char)
                             (looking-at "\\sw\\|\\s_"))))
          (forward-char)))
      (if (not (looking-at pattern)) nil
        (re-search-forward pattern)
        (setq found-string (match-string-no-properties 0)))
      (goto-char (if reverse (match-beginning 0) (match-end 0))))
    
    found-string))

(provide 'my-lisp)

;; Local Variables:
;; no-byte-compile: t
;; End:
