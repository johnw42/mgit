;; -*- no-byte-compile: t -*-
(require 'unit-test-core)
(require 'save-global-state)
(require 'my-util)

(defconst global1 'old-global1)
(defconst global2 'old-global2)
(defvar void1)
(defvar void2)

(unit-test-suite save-global-state-test
  (set-up
   (unintern "not-interned")
   (dolist (name '(local1 local2 void1 void2))
     (makunbound name)))

  (test-intern-1
   (save-global-state
    nil
    (assert-not (intern-soft "not-interned"))
    (intern "not-interned")
    (assert (intern-soft "not-interned")))
   (assert-not (intern-soft "not-interned")))

  ;; (test-intern-2
  ;;  (save-global-state
  ;;   nil
  ;;   (assert-not (intern-soft "not-interned"))
  ;;   (eval (read "(defvar not-interned)"))
  ;;   (assert (intern-soft "not-interned"))
  ;;   (assert-equal '((unintern '"not-interned")) sgs-undo-exprs))
  ;;  (assert-not (intern-soft "not-interned")))

  (test-restore-globals-1
   (save-global-state
    ()
    (set 'global1 'new-global1)
    (setq global2 'new-global2))
   (assert-eq 'old-global1 global1)
   (assert-eq 'old-global2 global2))

  (test-restore-globals-2
   (save-global-state
    ()
    (setq global1 1
          global1 2
          global1 3)
    (assert-equal 3 global1))
   (assert-eq 'old-global1 global1))

  (test-restore-void
   (save-global-state
    ()
    (set 'void1 t)
    (setq void2 t))
   (assert-not (boundp 'void1))
   (assert-not (boundp 'void2)))

  (test-space-complexity
   (save-global-state
    ()
    (dotimes (i 1000)
      (setq global1 i))
    (assert-equal 999 global1)
    (assert (> 50 (length sgs-undo-exprs))))
   (assert-equal 'old-global1 global1)))

;; Local Variables:
;; no-byte-compile: t
;; End:
