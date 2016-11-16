(defun count-occur(s lst)
	(cond
		((not (atom s)) "Error: s is not an atom.")
		((not (listp lst)) "Error: lst is not a list.")
		((equal lst NIL) 0)
		((atom (car lst)) 
			(if (equal (car lst) s) 
				(+ 1 (count-occur s (cdr lst))) 
				(count-occur s (cdr lst))))
		(t (+ (count-occur s (car lst)) (count-occur s (cdr lst))))))

(defun subexpr(expr1 expr2)
	(cond
		((equal expr1 expr2) t) 
		((null expr2) NIL)
		((atom expr2) NIL)
		;;Comment: if expr2 is atom then it is equal to expr1 or the 
		;;answer is null
		((equal expr1 (car expr2)) expr2)
		(t
			(if 
				(null (subexpr expr1 (car expr2)))
				;;Comment: we need to find first occurence,
				;;so there is no reason to check cdr if it is 
				;;subexpression in car of the expr2
				(if 
					(not (null (cdr expr2)))
					(subexpr expr1 (cdr expr2))) 
				(subexpr expr1 (car expr2))))
		(t NIL)))

(defun my-flatten (lst)
	(cond 
		((listp lst)
			(mapcan 
				(lambda(x)
					(if (atom x) (list x) (my-flatten x)))
				lst))
		(t "lst is not a list")))

(defun my-intersection (l1 l2)
	(cond
		((and (listp l1) (listp l2))
			(if (or (null l1) (null l2))
				() 
				(if 
					(member (car l1) l2 :test #'equal) 
					(cons 
						(car l1) 
						(my-intersection (cdr l1) l2)) 
					(my-intersection (cdr l1) l2))))
		(t "Error, not all input arguments are lists.")))

(defun fib (n)
	(cond
		((and (> n -1) (integerp n))
			(setf f1 1)
			(setf f2 1)
			(dotimes (i (- n 1))
				(setf f1 (+ f1 f2))
				(setf f2 (- f1 f2)))
			f1)
		(t "Error: n < 0 or n is not an integer")))

(defun merge-occurrence-counts (lst1 lst2)
	(labels 
		(
			;;Comment: equaltity function for pairs (x y), which
			;;compares by y only
			(my-equal(x y) 
				(if (equal (last x) (last y)) (car y) NIL)))
		(cond
			((null lst2)
				lst1)
			((null lst1)
				lst2)
			(t
				(if 
					(member (car lst1) lst2 :test #'my-equal) 
					;;Comment: case where pair (* y) is in both lists.
					(cons 
						(list 
							(+ 
								(car 
									(car lst1)) 
								(car 
									(car 
										(member 
											(car lst1) 
											lst2 
											:test #'my-equal)))) 
							(nth 1 (car lst1)))
						;;Comment: next recurrent step runs with lst2
						;;without pair which was merged in this step.
						;;Reason: if in the end of all process lst2 
						;;will have pairs, with entity name which is 
						;;not in lst1 pairs than it will be enough 
						;;just put all the pairs left in lst2 to the 
						;;new merged list.
						(merge-occurrence-counts 
							(cdr lst1) 
							(remove 
								(last (car lst1)) 
								lst2 
								:test #'my-equal)))
					;;Comment: case where pair`s name from lst1 is not 
					;;in the lst2. Just copy that pair to new merged
					;;list.
					(cons
						(car lst1)
						(merge-occurrence-counts (cdr lst1) lst2)))))))