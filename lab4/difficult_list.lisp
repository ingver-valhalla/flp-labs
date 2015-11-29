(defun dim (arr)
	(car (array-dimensions arr))
)

(defun set-arr-el (arr1 arr2 index)
	(let (
		(dummy
			(setf (aref arr2 index)
				(if (= (mod index 2) 0 )
					(cond
						((= 0 index) (print (aref arr1 (1+ index))) )
						((= (1- (dim arr1)) index) (print (aref arr1 (1- index))) )
						(t (+ (aref arr1 (1- index)) (aref arr1 (1+ index))) )
					)

					((lambda (x) (* x x)) (aref arr1 index))
				)
			)
		)
		)
		arr2
	)
)

(defun fill-arr (arr1 arr2 index)
	(cond
		((>= index (dim arr1)) arr2)
	    (t (fill-arr arr1 (set-arr-el arr1 arr2 index) (1+ index)) )
	)
)

(defun make-tricky-array (arr)
	(let
		((tricky (make-array (list (car (array-dimensions arr))) ) ))

		(labels (
			(make-tricky (arr1 arr2)
				(cond
					((not (= (dim arr1)
				             (dim arr1))) nil)

					(t (fill-arr arr1 arr2 0))
				)

			)

			)
			(list  arr (make-tricky arr tricky))
		)
	)
)

(make-tricky-array
    (make-array '(10)
        :initial-contents '(5 6 4 1 2 9 7 5 8 0)
    )
)