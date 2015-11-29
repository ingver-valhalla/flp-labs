(defun calc (fun-ls args) (cond
    ((or (null fun-ls) (null args)) nil)
    (t (cons (funcall (car fun-ls) (car args))
               (calc (cdr fun-ls) (cdr args)))
    )
  )
)


(defun test (fun-ls args results)
  (equal (calc fun-ls args) results)
)


(defun sum-ls (ls)
  (cond
    ((null ls) 0)
    (t (+ (car ls) (sum-ls (cdr ls))))
  )
)


(defun rvrs (ls)
  (cond
    ((null ls) nil)
    (t (append (reverse (cdr ls)) (list (car ls))))
  )
)



;; tests
(print
  (calc '(rvrs sum-ls) '((10 2 5) (5 7 2 11)))
)
(print
  (test '(rvrs sum-ls) '((10 2 5) (5 7 2 11)) '((5 2 10) 25))
)
(print
  (calc '(sum-ls rvrs) '((10 2 5) (5 7 2 11)))
)
(print
  (test '(sum-ls rvrs) '((10 2 5) (5 7 2 11)) '(17 (11 2 7 5)))
)
