(defun in (ls ls-of-ls)
  (cond
    ((null ls-of-ls) nil)
    ((equal ls (car ls-of-ls)) t)
    (t (in ls (cdr ls-of-ls)))
  )
)


(defun remove-dups (ls-of-ls)
  (let ((head (car ls-of-ls))
        (tail (cdr ls-of-ls)))
    (cond
      ((null ls-of-ls) '())
      ((in head tail) (remove-dups tail))
      (t (append (list head) (remove-dups tail)))
    )
  )
)
  

(defun ins-in-every-pos (el ls)
  (labels (
    (ins (1-el-ls head tail)
      (cond
        ((null tail) (list (append head 1-el-ls)))
        (t (append (list (append head 1-el-ls tail))
                   (ins 1-el-ls (append head (list (car tail))) (cdr tail))))
      )
    )
    )
    (ins (list el) nil ls)
  )
)
  
  
;(print
  ;(ins-in-every-pos 10 '(1 2 3))
;)


(defun ins-in-lists (el ls-of-ls)
  (cond
    ((null ls-of-ls) '())
    (t (append (ins-in-every-pos el (car ls-of-ls))
               (ins-in-lists el (cdr ls-of-ls))))
  )
)
  
  
;(print
  ;(ins-in-lists 10 '((4 2) (2 4)))
;)


(defun permuts (ls)
  (cond
    ((null ls) '(()))
    (t (remove-dups (ins-in-lists (car ls) (permuts (cdr ls)))))
  )
)
  
  
(print
  (permuts '(5 10 2 4))
)
