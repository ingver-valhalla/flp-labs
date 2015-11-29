; Задан интервал и шаг изменения аргумента. Вывести максимальное значение 
; функции y = 5·cos(3x) на заданном интервале и соответствующее ему значение
; аргумента.

(defun maximum (begin end step1)
	(labels (
	         (y (x) (* 5 (cos (* 3 x ))))
	         (maximum-aux (cur-arg cur-max)
	             (let ((cur-value (y cur-arg)))
	                  (cond ((> cur-arg end) 
	                            cur-max)
	                        ((> cur-value cur-max)
	                            (maximum-aux (+ cur-arg step1) cur-value))
	                        (t (maximum-aux (+ cur-arg step1) cur-max))
	                  )
	             )
	         )
	        )
	    (maximum-aux (+ begin step1) (y begin))
	)
)

;(print (cos 3.14 ))
;(print (* 5 (cos (* 3 5))))
(print (maximum 0.261799388 (- pi 0.261799388) 0.261799388) )
