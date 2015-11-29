(defun countp (pred ls)
  (cond
    ((null ls) 0)
    ((listp (car ls)) (+ (countp pred (car ls)) (countp pred (cdr ls))))
    ((funcall pred (car ls)) (1+ (countp pred (cdr ls))))
    (t (countp pred (cdr ls)))
  )
)

(trace countp)

(print
  (countp #'(lambda (x) (> x 3)) '(10 11 (1 2 3) ((5 2 9) 5 6 (3 2 1) (2 10 (1 1 1) 2 3 1) 1) 10 10))
)
