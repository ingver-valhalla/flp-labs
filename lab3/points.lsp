; Заданы координаты двух точек на плоскости. Определить квадранты, в которых
; расположены каждая из них и характер симметрии (относительно оси абсцисс,
; ординат, начала координат)



(defun pinfo (2points)
	(let ((p1 (car 2points))
	      (p2 (cadr 2points)))
	 
	    (labels (
	             (symmetry (p1 p2)
	                 (if (and (= (abs (car p1)) (abs (car p2))) 
	                          (= (abs (cadr p1)) (abs (cadr p2))))
	                     (cond ((and (= (car p1) (car p2))
                                         (= (cadr p1) (cadr p2))) 
	                           "Одна и та же точка")
                                   ((and (= (car p1) (- (car p2))) 
	                                 (= (cadr p1) (- (cadr p2)))) 
                                   "Симметрия относительно центра")
	                           ((= (car p1) (- (car p2))) 
	                            "Симметрия относительно OY")
	                           (t "Симметрия относительно OX" )
	                     )
	                     "Нет симметрии"
	
	                 )
	             )
	             (quadrant (p)
	                 (cond ((< 0 (car p))
	                           (cond ((< 0 (cadr p)) "Первый квадрант")
	                                 ((> 0 (cadr p)) "Четвертый квадрант")
	                                 (t "Ось X")))
	          
	                       ((> 0 (car p))
	                           (cond ((< 0 (cadr p)) "Второй квадрант")
	                           ((> 0 (cadr p)) "Третий квадрант")
	                           (t "Ось X")))
	      
	                       (t
	                           (cond ((= 0 (cadr p)) "Центр")
	                                 (t "Ось Y")))
	      
	                 )
	             )
	            )
	        (list (list 'Первая 'точка p1 (quadrant p1))
	              (list 'Вторая 'точка p2 (quadrant p2))
	              (symmetry p1 p2)
	        )
	    )
	)
)

(print (pinfo '((5 2) (5 2))))
