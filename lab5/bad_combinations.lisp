(defun combinations (n ls)
  (if (= n 1)
      (cond ((null ls) nil)
            ((null (cdr ls)) (list (list (car ls))))
            (t (cons (list (car ls)) (combinations n (cdr ls)))))
      (if (null (cdr ls))
          nil
          (append (ins (car ls) (combinations (1- n) (cdr ls))) (combinations n (cdr ls))))))

(defun ins (el lss)
  (if (null lss)
      nil
      (cons (cons el (car lss)) (ins el (cdr lss)))))

;(print (ins 3 '((1 2) (4 5) (6 7))))
(print (combinations 4 '(1 2 3 4)))
(print (combinations 1 '()))

