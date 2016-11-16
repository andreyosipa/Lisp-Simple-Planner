(defun count-occur(s lst)
	(cond
		((not (atom s)) (print "S is not an atom"))
		((not (listp lst)) (print "LST is not a list"))
		((equal lst NIL) 0)
		((atom (car lst)) 
			(if (equal (car lst) s) 
				(+ 1 (count-occur s (cdr lst))) 
				(count-occur s (cdr lst))))
		(t (+ (count-occur s (car lst)) (count-occur s (cdr lst))))))

(defun subexpr(expr1 expr2)
	(cond
		((equal expr1 expr2) t)
		((atom expr2) NIL)
		((equal expr1 (car expr2)) expr2)
		((and (not (null expr2)) (listp expr2)) 
			(if 
				(null (subexpr expr1 (car expr2))) 
				(subexpr expr1 (cdr expr2)) 
				(subexpr expr1 (car expr2))
				)
			)
		(t NIL)
	)	
)

(defun my-flatten (lst)
	(mapcan 
		(lambda(x)
			(if (atom x) (list x) (my-flatten x))
			)
		lst
		)
)

(defun my-intersection (l1 l2)
	(cond
		((and (list l1) (list l2))
			(if (or (null l1) (null l2))
				() 
				(if 
					(member (car l1) l2 :test #'equal) 
					(cons 
						(car l1) 
						(my-intersection (cdr l1) l2)
						) 
					(my-intersection (cdr l1) l2)))
		)
		(t (print "error"))
	)
)

(defun fib (n)
	(setf f1 1)
	(setf f2 1)
	(dotimes (i (- n 1))
		(setf f1 (+ f1 f2))
		(setf f2 (- f1 f2)))
	f1
)

(defun merge-occurence-counts (lst1 lst2)
	(labels 
		(
			(my-equal(x y) 
				(if (equal (last x) (last y)) (car y) NIL)
				)
			)
		(cond
			((null lst2)
				lst1
				)
			((null lst1)
				lst2
				)
			(t
				(if 
					(member (car lst1) lst2 :test #'my-equal) 
					(cons 
						(list 
							(+ 
								(car 
									(car lst1)
									) 
								(car 
									(car 
										(member 
											(car lst1) 
											lst2 
											:test #'my-equal
											)
										)
									)
								) 
							(last (car lst1))
							)
						(merge-occurence-counts 
							(cdr lst1) 
							(remove 
								(last (car lst1)) 
								lst2 
								:test #'my-equal
								)
							)
						)
					(cons
						(car lst1)
						(merge-occurence-counts (cdr lst1) lst2)
						)
					)
				)
			)
		)
	)