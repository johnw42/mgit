;; -*- lexical-binding: t -*-
(eval-when-compile (require 'pcase))
(require 'dash)
(with-no-warnings (require 'cl))

(cl-defstruct generator-bblock forms id successor)

(defun generator-bblock-progn (bblock)
  (let ((forms (generator-bblock-forms bblock)))
    (if (null (cdr forms))
        (car forms)
      `(progn ,@forms))))

(defmacro generator (&rest body)
  (assert lexical-binding)
  (let ((bblock-sym (cl-gensym "bblock"))
        (bblocks (generator-bblocks
                 `(progn ,@(mapcar 'macroexpand-all body)))))
    `(let ((,bblock-sym 0))
       (lambda ()
         (case ,bblock-sym
           ,@(--map
              `(,(generator-bblock-id it)
                (setq ,bblock-sym
                      ,(let ((succ (generator-bblock-successor it)))
                         (and succ (generator-bblock-id succ))))
                ,@(generator-bblock-forms it))
              (generator-number-cases bblocks 0))
           )))))

(defun generator-bblocks (form)
  (pcase form
    ((pred atom)
     (list (make-generator-bblock
            :forms (list form nil))))
    (`(progn . ,body-forms)
     (generator-progn-bblocks body-forms))
    (`(while . ,body-forms)
     (generator-while-bblocks body-forms))
    (`(yield . ,values)
     (list (make-generator-bblock
            :forms (list `(list ,@values)))))
    ((pred consp)
     `(error "Unsupported form: %S" form))))

(defun generator-progn-bblocks (body-forms)
  (let ((body-bblocks
         (--mapcat (generator-bblocks it) body-forms)))
    (generator-merge-bblocks
     body-bblocks)))

(defun generator-while-bblocks (forms)
  (let* ((pred-form (car forms))
         (body-forms (cdr forms))
         (pred-bblocks (generator-bblocks pred-form))
         (body-bblocks (generator-progn-bblocks body-forms)))
    (if (or t (and (null (cdr pred-bblocks))
                  (null (cdr body-bblocks))))
        (make-generator-bblock
         :forms (list `(while ,(generator-bblock-progn (car pred-bblocks))
                         ,@(generator-bblock-forms (car body-bblocks)))))
      nil))
  )

(defun generator-merge-bblocks (bblock-list)
  (if (null bblock-list) nil
    (let* ((head (car bblock-list))
           (head-forms (generator-bblock-forms head))
           (merged-tail (generator-merge-bblocks (cdr bblock-list))))
      (if (and (null (car (last head-forms)))
               merged-tail)
          (cons (make-generator-bblock
                 :forms (append (butlast head-forms)
                                (generator-bblock-forms (car merged-tail))))
                (cdr merged-tail))
        (cons head merged-tail)))
    ))

;; (defun generator-progn-bblocks (body-forms)
;;   (let (bblocks bblock-forms)
;;     (dolist (form (append body-forms '((yield))))
;;       (if (not (generator-yield-p form))
;;           (let ((form-bblocks (generator-bblocks form)))
;;             )
;;           (push form bblock-forms)
;;         (push form bblock-forms)
;;         (push (nreverse bblock-forms) bblocks)
;;         (setq bblock-forms nil)))
;;     (nreverse bblocks)))

(defun generator-number-cases (bblocks start-index)
  (let ((index start-index)
        prev-bblock)
    (dolist (bblock bblocks)
      (setf (generator-bblock-id bblock) index)
      (when prev-bblock
        (setf (generator-bblock-successor prev-bblock) bblock))
      (setq prev-bblock bblock)
      (incf index)))
  bblocks)

(defun generator-yield-p (form)
  (eq 'yield (car-safe form)))

(defun generator-split-at (pred forms)
  (and forms
       (let ((head (car forms))
             (tail (generator-split-at pred (cdr forms))))
         (if (funcall pred head)
             (cons (list head) tail)
           (cons (cons head (car tail)) (cdr tail))))))

(eval-when (eval)
  (generator-split-at 'evenp '(1 1 2 3 4 6 5 5 5 8 9)))

(eval-when (load eval)
  (run-with-timer nil nil
                  'pp-macroexpand-expression
                  '(generator
                    a1
                    b1
                    (while p
                      a2
                      b2
                      ;;(yield 2)
                      c2
                      d2)
                    c1
                    d1)))

;; (eval-when (load eval)
;;   (message "bar")
;;   (run-with-timer
;;    nil nil
;;    'pp-macroexpand-expression
;;    '(generator
;;      a1
;;      b1
;;      (yield 1)
;;      (progn
;;        a2
;;        b2
;;        (yield 2)
;;        c2
;;        d2)
;;      c1
;;      d1)))

(eval-when-compile
  '(let ((g (generator (yield 1) (yeild 2) (yield 3))))
    (assert (equal (funcall g) '(1)))
    (assert (equal (funcall g) '(2)))
    (assert (equal (funcall g) '(3)))
    (assert (equal (funcall g) nil))))  

(provide 'generator)
