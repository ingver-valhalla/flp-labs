; Заданы координаты вершин четырехугольника. Определить его тип: прямоугольник,
; параллелограмм, трапеция, квадрат, произвольный.


(defun quadrangleType (coords)
	(labels 
	    (
	        ; Определение взаимное расположение
	        ; Если flag = 1, определяется параллельность,
	        ; если 0 -- перпендикулярность
	        (arrangement (4coord flag)
	            (labels 
	                (
	                    ; Получение вектора из координат точек
	                    (get-vec (a b)
	                        (list 
	                            (- (car b) (car a))
	                            (- (cadr b) (cadr a))
	                        )
	                    )
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
	                (let ((a (get-vec (car 4coord) (cadr 4coord)))
	                      (b (get-vec (caddr 4coord) (cadddr 4coord)))
	                     )
	                     ; flag * |a|*|b| = |a1*b1 + a2*b2|
	                     (=  (* flag (ceiling (* (vec-len a) (vec-len b)))) 
	                         (abs (scalar a b)))
	                )
	            )
	        )
	        ; Определение параллельности отрезков
	        (parallel (4coord)
	            (arrangement 4coord 1)
	        )
	        ; Определение перпендикулярности отрезков
	        (perpendicular (4coord)
	            (arrangement 4coord 0)
	        )
	        ; Определение равности сторон
	        (eq-s (a b c d)
	            (= (vec-len (get-vec a b)) (vec-len (get-vec c d)))
	        )
	    )
	    (if (parallel coords) 
	        ; Проверка перепендикулярности смежных сторон
	        (if (perpendicular (list (car coords) (cadr coords)
	                                  (cadr coords) (caddr coords)))
	            (if (eq-s (car coords) (cadr coords)
	                      (cadr coords) (caddr coords) )
	                'квадрат
	                'прямоугольник
	            )
	            (if (parallel (list (cadr coords) (caddr coords)
	                                (cadddr coords) (car coords)))
	                'параллелограмм
	                'трапеция
	            )
	        )
	        (if (parallel (list (cadr coords) (caddr coords) 
	                            (cadddr coords) (car coords)))
	            'трапеция
	            'произвольный
	        )
	   )
	)
)
(print (quadrangleType '((1 0)(2 5)(5 7)(4 2))))
(print (quadrangleType '((1 1)(5 6)(12 7)(8 3))))
(print (quadrangleType '((1 1)(5 6)(12 8)(8 3))))
(print (quadrangleType '((5 6)(12 8)(8 3)(1 1))))
