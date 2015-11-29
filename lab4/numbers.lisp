; Заданы количество цифр числа и их сумма. Найти все числа, удовлетворяющие
; этому условию.


(defun get-max (n)
	(cond 
		((= n 0) 0 )
		(t (+ (* 9 (expt 10 (- n 1))) (get-max (- n 1))))
	)
)

(defun get-sum (n)
	(cond
		((= n 0) 0)
		(t (+ (- n (* 10 (floor n 10))) (get-sum (floor n 10))))
	)
)

; (defun eq-sum (sum1 sum2)
	; (if (= sum1 sum2) (list n) nil)
; )

(defun eq-sum-of-digits (cur max sum &optional res)
 
	(cond
		((= max cur) res)
		(t (eq-sum-of-digits (+ cur 1) max sum (if (= sum (get-sum cur)) (append res (list cur)) res)))
	)
)


(defun nums (digits sum)
	(let ((max (get-max digits)))
		(cond
			((or (= digits 0) (= sum 0)) (list 0))
			(t (eq-sum-of-digits 0 max sum nil))
		)
	)
)

(nums 4 18)