;; -*- no-byte-compile: t -*-
(require 'cl)
(require 'dabbrev)

;;;---------------------------------------------------------------------
;;; Extension to dabbrev allowing customized abbreviation search
;;; function.
;;;

(defvar dabbrev-search-function
  nil
  "*Either nil or a function used to find possible expresions of
a dynamic abbreviation.

The function should accept two arguments, ABBREV and REVERSE,
where ABBREV is the abbreviation to expand and REVERSE indicates
the search direction (forward if nil, backward if non-nil).

When successful, the function should return a expansion of ABBREV
and move the point to the location where the expresion was found.
When unsuccessful, the function should return nil.")

(make-variable-buffer-local 'dabbrev-search-function)

;; Function factored out of the original dabbrev--search.
;;;###autoload
(defun dabbrev--default-search-function (abbrev reverse)
  "The default search function to use when
`dabbrev-search-function' is nil."
  (let ((pattern1 (concat (regexp-quote abbrev)
                          "\\(" dabbrev--abbrev-char-regexp "\\)"))
        (pattern2 (concat (regexp-quote abbrev)
                          "\\(\\(" dabbrev--abbrev-char-regexp "\\)+\\)"))
        found-string)
    (while (and (not found-string)
                (if reverse
                    (re-search-backward pattern1 nil t)
                  (re-search-forward pattern1 nil t)))

      (goto-char (match-beginning 0))

      ;; In case we matched in the middle of a word,
      ;; back up to start of word and verify we still match.
      (dabbrev--goto-start-of-abbrev)

      (if (not (looking-at pattern1))
          nil
        ;; We have a truly valid match.  Find the end.
        (re-search-forward pattern2)
        (setq found-string (match-string-no-properties 0)))
    
      ;; Prepare to continue searching.
      (goto-char (if reverse (match-beginning 0) (match-end 0))))

    found-string))

;;;###autoload (require 'redefun) (redefun dabbrev--search my-dabbrev--search)
;;;###autoload
(defun my-dabbrev--search (abbrev reverse ignore-case)
  "Use dabbrev-search-function instead of the default search
heuristic."
  (save-match-data
    (let ( ;; This makes it possible to find matches in minibuffer prompts
          ;; even when they are "inviolable".
          (inhibit-point-motion-hooks t)
          found-string result)
      ;; Limited search.
      (save-restriction
        (and dabbrev-limit
             (narrow-to-region dabbrev--last-expansion-location
                               (+ (point)
                                  (if reverse (- dabbrev-limit) dabbrev-limit))))
        ;;--------------------------------
        ;; Look for a distinct expansion, using dabbrev--last-table.
        ;;--------------------------------
        (while (and (not found-string)
                    (setq found-string
                          (if dabbrev-search-function
                              (funcall dabbrev-search-function abbrev reverse)
                            (dabbrev--default-search-function abbrev reverse))))
          (setq result found-string)
          (and ignore-case (setq found-string (downcase found-string)))
          ;; Ignore this match if it's already in the table.
          (if (dabbrev-filter-elements
               table-string dabbrev--last-table
               (string= found-string table-string))
              (setq found-string nil)))

        ;; If we found something, use it.
        (when found-string
          ;; Put it into `dabbrev--last-table'
          ;; and return it (either downcased, or as is).
          (setq dabbrev--last-table
                (cons found-string dabbrev--last-table))
          result)))))


;;;
;;; Add option to allow expansion of empty abbreviations.
;;;

(defvar dabbrev-allow-empty-abbrev nil
  "*If true, allow empty abbreviations to be exanded.")

;;;###autoload (require 'redefun) (redefun dabbrev-expand my-dabbrev-expand)
;;;###autoload
(defun my-dabbrev-expand (arg)
  "Allow expansion of an emtpy abbreviation."
  ;;(message "args = (%S . %S)" arg rest)
  (let (abbrev record-case-pattern
               expansion old direction (orig-point (point)))
    ;; abbrev -- the abbrev to expand
    ;; expansion -- the expansion found (eventually) or nil until then
    ;; old -- the text currently in the buffer
    ;;    (the abbrev, or the previously-made expansion)
    (save-excursion
      (if (and (null arg)
               (markerp dabbrev--last-abbrev-location)
               (marker-position dabbrev--last-abbrev-location)
               (or (eq last-command this-command)
                   (and (window-minibuffer-p (selected-window))
                        (= dabbrev--last-abbrev-location
                           (point)))))
          ;; Find a different expansion for the same abbrev as last time.
          (progn
            (setq abbrev dabbrev--last-abbreviation)
            (setq old dabbrev--last-expansion)
            (setq direction dabbrev--last-direction))
        ;; If the user inserts a space after expanding
        ;; and then asks to expand again, always fetch the next word.
        (if (and (not dabbrev-allow-empty-abbrev)
                 (eq (preceding-char) ?\s)
                 (markerp dabbrev--last-abbrev-location)
                 (marker-position dabbrev--last-abbrev-location)
                 (= (point) (1+ dabbrev--last-abbrev-location)))
            (progn
              ;; The "abbrev" to expand is just the space.
              (setq abbrev " ")
              (save-excursion
                (save-restriction
                  (widen)
                  (if dabbrev--last-buffer
                      (set-buffer dabbrev--last-buffer))
                  ;; Find the end of the last "expansion" word.
                  (if (or (eq dabbrev--last-direction 1)
                          (and (eq dabbrev--last-direction 0)
                               (< dabbrev--last-expansion-location (point))))
                      (setq dabbrev--last-expansion-location
                            (+ dabbrev--last-expansion-location
                               (length dabbrev--last-expansion))))
                  (goto-char dabbrev--last-expansion-location)
                  ;; Take the following word, with intermediate separators,
                  ;; as our expansion this time.
                  (re-search-forward
                   (concat "\\(?:" dabbrev--abbrev-char-regexp "\\)+"))
                  (setq expansion (buffer-substring-no-properties
                                   dabbrev--last-expansion-location (point)))

                  ;; Record the end of this expansion, in case we repeat this.
                  (setq dabbrev--last-expansion-location (point))))
              ;; Indicate that dabbrev--last-expansion-location is
              ;; at the end of the expansion.
              (setq dabbrev--last-direction -1))

          ;; We have a different abbrev to expand.
          (dabbrev--reset-global-variables)
          (setq direction (if (null arg)
                              (if dabbrev-backward-only 1 0)
                            (prefix-numeric-value arg)))
          (setq abbrev (if (and dabbrev-allow-empty-abbrev
                                (eq (preceding-char) ?\s))
                           ""
                         (dabbrev--abbrev-at-point)))
          (setq record-case-pattern t)
          (setq old nil)))

      ;;--------------------------------
      ;; Find the expansion
      ;;--------------------------------
      (or expansion
          (setq expansion
                (dabbrev--find-expansion abbrev direction
                                         (and (if (eq dabbrev-case-fold-search 'case-fold-search)
                                                  case-fold-search
                                                dabbrev-case-fold-search)
                                              (or (not dabbrev-upcase-means-case-search)
                                                  (string= abbrev (downcase abbrev))))))))
    (cond
     ((not expansion)
      (dabbrev--reset-global-variables)
      (if old
          (save-excursion
            (setq buffer-undo-list (cons orig-point buffer-undo-list))
            ;; Put back the original abbrev with its original case pattern.
            (search-backward old)
            (insert abbrev)
            (delete-region (point) (+ (point) (length old)))))
      (error "No%s dynamic expansion for `%s' found"
             (if old " further" "") abbrev))
     (t
      (if (not (or (eq dabbrev--last-buffer dabbrev--last-buffer-found)
                   (minibuffer-window-active-p (selected-window))))
          (progn
            (message "Expansion found in '%s'"
                     (buffer-name dabbrev--last-buffer))
            (setq dabbrev--last-buffer-found dabbrev--last-buffer))
        (message nil))
      (if (and (or (eq (current-buffer) dabbrev--last-buffer)
                   (null dabbrev--last-buffer))
               (numberp dabbrev--last-expansion-location)
               (and (> dabbrev--last-expansion-location (point))))
          (setq dabbrev--last-expansion-location
                (copy-marker dabbrev--last-expansion-location)))
      ;; Success: stick it in and return.
      (setq buffer-undo-list (cons orig-point buffer-undo-list))
      (dabbrev--substitute-expansion old abbrev expansion
                                     record-case-pattern)

      ;; Save state for re-expand.
      (setq dabbrev--last-expansion expansion)
      (setq dabbrev--last-abbreviation abbrev)
      (setq dabbrev--last-abbrev-location (point-marker))))))

(provide 'my-dabbrev)

;; Local Variables:
;; no-byte-compile: t
;; End:
