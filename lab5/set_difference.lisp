(defun in (set1 el)
  (cond
    ((null set1) nil)
    ((= (car set1) el) t)
    (t (in (cdr set1) el))
  )
)

(defun remove-dup (set1 result)
  (cond
    ((null set1) result)
    ((in result (car set1)) (remove-dup (cdr set1) result))
    (t (remove-dup (cdr set1) 
                   (append result (list (car set1))))
    )
  )
)

(defun set-diff (dup-set1 dup-set2)
  (labels (
    (set-diff1 (set1 set2 result)
      (cond
        ((null set2) set1)
        ((null set1) result)
        ((in set2 (car set1)) (set-diff1 (cdr set1) set2 result))
        (t (set-diff1 (cdr set1) set2 (append result (list (car set1)))))
      )
    )
    )

    (let ((set1 (remove-dup dup-set1 nil)) (set2 (remove-dup dup-set2 nil)))
      (set-diff1 set1 set2 nil)
    )
  )
)

(print (set-diff '(5 8 12 1 2 15) '(1 2 8 10)))
(print (set-diff '(100 15 2 75 6 81 2 8 1) '(654 74 12 2 15 81 8)))
(print (set-diff '() '(1 2 3)))
(print (set-diff '(1 2 3) '()))
(print (set-diff '() '()))
