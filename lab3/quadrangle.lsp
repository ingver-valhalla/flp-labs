; Заданы координаты вершин четырехугольника. Определить его тип: прямоугольник,
; параллелограмм, трапеция, квадрат, произвольный.

(defun parallel (4coord)
	(labels ( 
		  ; Вычисление длины вектора вида (x y)
		  (vec-len (coord)
		     (abs (complex (car coord) (cadr coord)))
		  )
		  ; Вычисление скалярного произведения
		  (scalar (a b)
		     (+ (* (car a) (car b))(* (cadr a) (cadr b)))
		  )
		)
		; Создаем векторы вида (x y) из заданных координат
		(let ((a (list (- (cadadr 4coord) (cadar 4coord)) 
			       (- (caadr 4coord) (caar 4coord))))
		      (b (list (- (cadr (cadddr 4coord)) (car (cdaddr 4coord))) 
			       (- (car (cadddr 4coord)) (caaddr 4coord))))
		     )
		     ; |a|*|b| = a1*b1 + a1*b2
		     (=  (ceiling (* (vec-len a) (vec-len b))) (scalar a b))
		)
	)
)
	
(print (parallel '((1 2)(3 4)(6 5)(8 7))))
