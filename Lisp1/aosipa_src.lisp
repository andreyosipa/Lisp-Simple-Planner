(defun unifier(lst1 lst2)
	;;Input:  two lists of equal length, where first contains only
	;;		  constants or variables and second constants only.
	;;Output: list of pairs (?variable constant) or NIL if input is
	;;  	  wrong or NIL.
	(labels 
		(
			(check-atoms(lst)
				;;Function to check wether list contains only atoms.
				;;Input:  list.
				;;Output: boolean.
				(equal
					(remove-if
						(lambda(x)
							(not (atom x)))
						lst)
					lst))
			(has-variables(lst)
				;;Function to check wether list contains variables.
				;;Input:  list.
				;;Output: boolean.
				(not
					(equal
						(remove-if
							(lambda(x)
								(if (not (integerp x)) 
									(equal 
										(subseq (string x) 0 1) 
										"?")
									(equal 
										(subseq (write-to-string x) 0 1) 
										"?")))
							lst)
						lst)))
			(my-check(lst)
				;;Function to check wether list of unifiers(list of size 
				;;2) contains unifiers for same variable.
				;;Input:  list
				;;Output: boolean.
				(cond
					((null lst) T)
					((equal 
						(delete-duplicates
							lst
							:test (lambda(x y) (equal (car x) (car y))))
						lst)
						lst)
					(T NIL)))
			(ind-unif(x)
				;;Function to check wether given list is a unifier: list 
				;;of length 2 that contains variable on the first place.
				;;Input:  list.
				;;Output: boolean.
				(and
					(equal (length x) 2)
					(and 
						(atom (nth 1 x))
						(equal 
							(subseq (write-to-string (car x)) 0 1) 
							"?"))))
			(if-unifier(x) 
				;;Function to check wether given list is a list of
				;;unifiers.
				;;Input:  list.
				;;Output: boolean.
				(cond
					((null x) T)
					((not (listp x)) NIL)
					((listp (car x)) 
						(and 
							(ind-unif (car x)) 
							(if-unifier (cdr x)))))))
		(cond
			;;Wrong input: not lists.
			((or (not (listp lst1)) (not (listp lst2))) NIL)
			;;Wrong input: null(s).
			((or (null lst1) (null lst2)) NIL)
			;;Wrong input: different count of vars and values.
			((not (equal (length lst1) (length lst2))) NIL)
			;;Standard case: lists of atoms, where only one has 
			;;variables in it.
			(
				(and 
					(check-atoms lst1) 
					(and 
						(check-atoms lst2)
						(not (has-variables lst2))))
				;;Chech wether there are unifiers for same variable
				;;but different values.
				(my-check
					;;List of quantifiers without equal elements. 
					;;Therefore only unique unifiers. 
					(delete-duplicates 
						;;List of unifiers without unifiers of form
						;;(const const) for same const.
						(remove-if 
							(lambda(x)
								(if (equal (car x) (second x)) T NIL))
							(mapcar
								(lambda(x y) (list x y))
								lst1 lst2))
						:test #'equal)))
			(T NIL))))

(defun apply-unifier(unif lit)
	;;Input:  list of unifiers(list of pairs (?var const)) and list 
	;;		  with correct literal. Correct literal must have form 
	;;		  (fun_name literal/variable/constant+).
	;;Output: Substitution of variables with corresponding 
	;;		  constants or NIL if input is wrong or NIL.
	(labels
		(
			(magic(x y)
				;;Function that takes unifier (?name value) and part of
				;;literal and if this part of literal is ?name then
				;;returns value, else returns same part.
				(if (equal (car y) x)  (second y) x))
			(if-literal(x)
				;;Function to check if x is literal.
				(cond
					((not (listp x)) T)
					;;Literal must have name of function as first part
					;;therefore it must be an atom.
					((not (atom (car x))) NIL)
					;;First part of literal cant be variable.
					((not (equal 
								(subseq (write-to-string (car x)) 0 1)
								"?"))
						(not (member
								NIL
								;;Taking every part of x and checking 
								;;if if list(therefore it must be
								;;literal itself) or whether it is 
								;;variable.
								(mapcar
									(lambda(y)
										(cond
											((listp y) (if-literal y))
											(T T)
										))
									(cdr x)))))
					(T NIL)))
			(ind-unif(x)
				;;See description in "unifier" function.
				(and
					(equal (length x) 2)
					(and 
						(atom (nth 1 x))
						(equal 
							(subseq (write-to-string (car x)) 0 1) 
							"?"))))
			(if-unifier(x) 
				;;See description in "unifier" function.
				(cond
					((null x) T)
					((not (listp x)) NIL)
					((listp (car x)) 
						(and 
							(ind-unif (car x)) 
							(if-unifier (cdr x)))))))
		(cond 
			((or (null unif) (null lit)) lit)
			((and (if-unifier unif) (if-literal lit))
				(apply-unifier
					(cdr unif)
					;;Applying first individual unifier to the literal.
					(loop for w in lit
						collect (if 
									(atom w) 
									(magic w (car unif))
									(apply-unifier unif w)))))
			(T NIL))))

(defun extract-constants(lits)
	;;Input:  list of states or premises. (In other words: list of literals 
	;;		  with constants and variables).
	;;Output: List of all individual constants names or NIL if there 
	;;		  is no individual constants or input is wrong.
	(labels
		(
			(if-literal(x)
				(cond
					((not (listp x)) T)
					;;Literal must have name of function as first part
					;;therefore it must be an atom.
					((not (atom (car x))) NIL)
					;;First part of literal cant be variable.
					((not (equal 
							(subseq (write-to-string (car x)) 0 1) 
							"?"))
						(not (member
								NIL
								;;Taking every part of x and checking 
								;;if if list(therefore it must be
								;;literal itself) or whether it is 
								;;variable.
								(mapcar
									(lambda(y)
										(cond
											((listp y) (if-literal y))
											(T T)
										))
									(cdr x)))))
					(T NIL))))
	(cond
		((null lits) NIL)
		((if-literal lits)
			(delete-duplicates
				(mapcan
					(lambda(x)
						(cond
							((and 
								(atom x) 
								(not (equal 
										(subseq (write-to-string x) 0 1) 
										"?"))) 
								(list x))
							((atom x) NIL)
							(T (extract-constants x))
						))
					(cdr lits))
				:test #'equal))
		((if-literal (car lits))
			(delete-duplicates
				(append
					(extract-constants (car lits))
					(extract-constants (cdr lits)))
				:test #'equal))
		(T NIL))))

(defun prune-unifiers(unifs)
	;;Input:  List of lists of pairs (?var const).
	;;Output: List of lists of pairs (?var const) such that lists 
	;;		  with pairs that have same constants for different 
	;;		  variables are excluded or NIL if input is wrong.
	(labels
		(
			;;See description in "unifier" function.
			(ind-unif(x)
				(and
					(equal (length x) 2)
					(and 
						(atom (nth 1 x))
						(equal 
							(subseq (write-to-string (car x)) 0 1) 
							"?"))))
			;;See description in "unifier" function.
			(if-unifier(x) 
				(cond
					((null x) T)
					((not (listp x)) NIL)
					((listp (car x)) 
						(and 
							(ind-unif (car x)) 
							(if-unifier (cdr x)))))))
		(cond 
			((null unifs) NIL)
			((if-unifier unifs)
				(if
					(equal 
						;;List of unifiers without completely equal
						;;ones.
						(delete-duplicates
							unifs
							:test #'equal)
						;;List of unifiers without ones that apply
						;;values to same variables. Therefore this
						;;list is shorter or equal that upper one. 
						;;If it is shorter than there were bad 
						;;unifiers.
						(delete-duplicates
							unifs
							:test
							(lambda(x y)
								(equal (cdr x) (cdr y))))
					)
					;;If there were no bad unifiers then keep this 
					;;list of unifiers. Else drop it and continue for
					;;next lists.
					(delete-duplicates
						unifs
						:test #'equal)))
			((if-unifier (car unifs))
				(if
					(equal 
						;;List of unifiers without completely equal
						;;ones.
						(delete-duplicates
							(car unifs)
							:test #'equal)
						;;List of unifiers without ones that apply
						;;values to same variables. Therefore this
						;;list is shorter or equal that upper one. 
						;;If it is shorter than there were bad 
						;;unifiers.
						(delete-duplicates
							(car unifs)
							:test
							(lambda(x y)
								(equal (cdr x) (cdr y))))
					)
					;;If there were no bad unifiers then keep this 
					;;list of unifiers. Else drop it and continue for
					;;next lists.
					(cons
						(delete-duplicates
							(car unifs)
							:test #'equal)
						(prune-unifiers (cdr unifs)))
					(prune-unifiers (cdr unifs)))))))	
