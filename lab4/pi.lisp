(defun set-arr-el (arr index)
	(let ((dummy (setf (aref arr index)
	                  (let ((val (/ 4 (1+ (* index 2)))))
			              (if (= (mod index 2) 0) val (- val)))
		         )))
	    arr
	)
)

(defun fill-arr (arr index size)
	(cond ((>= index size) arr)
	      (t (fill-arr (set-arr-el arr index) (1+ index) size))
	)
)

(defun sum-arr (arr size)
	(labels 
		((sum-arr1 (arr index size summa)
			(cond ((>= index size) summa)
			      (t (sum-arr1 arr (1+ index) size (+ summa (aref arr index))) )
			)
		 ))
		(sum-arr1 arr 0 size 0)
	)
)

(defun expand-pi (n)
	(let ((result-arr (fill-arr (make-array (list n)) 0 n)))
		(list
			result-arr
			(sum-arr result-arr n)
		)
	)	
)

(expand-pi 5)

