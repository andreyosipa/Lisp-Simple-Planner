;;Test Framework from Peter Siebel "Practical Common Lisp"
(defvar *test-name* nil)

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
    ,@body))

(defmacro deftest (name parameters &body body)
;;Define a test function. Within a test function we can call other
;;test functions or use `check' to run individual test cases.
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  ;;Run each expression in `forms' as a test case.
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  ;;Combine the results (as booleans) of evaluating `forms' in order.
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  ;;Report the results of a single test case. Called by `check'.
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;;TESTS

(deftest test-distinct-bindings ()
	(format t "Test function for distinct-bindings.~%Cases #1-3 are ~
		with good input variables and constants.~%Case #4 has more ~
		variables then constants, so there is no possible bindings as ~
		we do not have one value assigned to different variables.~
		~%Cases #5-6 have NIL(s) in input.~%Cases #7-10 have some bad ~
		input.~%")
	(check
		(equal 
			(distinct-bindings '(?x ?y) '(A B Table))
			'(((?x A) (?y B))
			  ((?x A) (?y Table))
			  ((?x B) (?y A))
			  ((?x B) (?y Table))
			  ((?x Table) (?y A))
			  ((?x Table) (?y B))))
		(equal
			(distinct-bindings '(?x) '(a))
			'(((?x a))))
		(equal
			(distinct-bindings '(?x ?y) '(A B))
			'(((?x A) (?y B)) ((?x B) (?y A))))
		(equal
			(distinct-bindings '(?x ?y) '(a))
			NIL)
		(equal
			(distinct-bindings '(?x ?x) '(A B Table))
			NIL)
		(equal
			(distinct-bindings NIL '(A B Table))
			NIL)
		(equal
			(distinct-bindings '(?x ?y) NIL)
			NIL)
		(equal
			(distinct-bindings '(?c d (etc)) '(A B Table))
			NIL)
		(equal
			(distinct-bindings 'a '(A B Table))
			NIL)
		(equal
			(distinct-bindings '(?x ?y) '(and some (?crap here)))
			NIL)
		(equal
			(distinct-bindings '(?x ?y) 'a)
			NIL)
	)
	(format t "~%"))

(deftest test-is-state-lst? ()
	(format t "Test for function is-state-lst?.~%First 3 cases are ~
		well fromed lists of states.~%Case #4 contains minor mistake ~
		- variable inside one of states.~%Case #5 is some bad list.~
		~%Case #6 is not a list.~%Case #7 is NIL.~%")
	(check
		(equal (is-state-lst? '((on A B) (on B Table) (clear X))) T)
		(equal (is-state-lst? '((empty-ferry))) T)
		(equal (is-state-lst? '((empty-ferry k))) T)
		(equal (is-state-lst? '((on A B) (on ?B Table) (clear X))) NIL)
		(equal (is-state-lst? '(some ((bad) list) ?with (?trash) t)) NIL)
		(equal (is-state-lst? 'a) NIL)
		(equal (is-state-lst? NIL) T))
	(format t "~%"))

(deftest test-is-operator? ()
	(format t "Test for function is-operator?.~%Cases #1-4 are some  ~
		correctt examples from emqail with task clarifications.~
		~%Cases #5-9 are negative examples with mistakes in one of ~
		the parts of the definition of an operator.~%")
	(check
		(equal 
			(is-operator? '(board (?x ?y)
    						 ((auto ?x)
     						 (place ?y)
     						 (at ?x ?y)
      						 (at-ferry ?y)
      						 (empty-ferry))
     						 ((on ?x Ferry)
     						 (not (at ?x ?y))
      						 (not (empty-ferry)))))
			T)
		(equal
			(is-operator? '(move-disk (?disk ?below-disk ?new-below-disk)
     						((disk ?disk)
      						(smaller ?disk ?new-below-disk)
      						(on ?disk ?below-disk)               
     						(clear ?disk)
      						(clear ?new-below-disk))
     						((clear ?below-disk)
      						(on ?disk ?new-below-disk)
      						(not (on ?disk ?below-disk))
      						(not (clear ?new-below-disk)))))
			T)
		(equal
			 (is-operator? '(sail (?x ?y)
     						((place ?x)
      						(place ?y)
      						(at-ferry ?x))
     						((at-ferry ?y)
      						(not (at-ferry ?x)))))
			T)
		(equal
			 (is-operator? '(sail (?x ?y)
     						((place x)
      						(place ?y)
      						(at-ferry ?x))
     						((at-ferry ?y)
      						(not (at-ferry ?x)))))
			T)
		(equal
			 (is-operator? '(?sail (?x ?y)
     						((place x)
      						(place ?y)
      						(at-ferry ?x))
     						((at-ferry ?y)
      						(not (at-ferry ?x)))))
			NIL)
		(equal
			 (is-operator? '(sail (x y)
     						((place ?x)
      						(place ?y)
      						(at-ferry ?x))
     						((at-ferry ?y)
      						(not (at-ferry ?x)))))
			NIL)
		(equal
			 (is-operator? '(sail ?x ?y
     						((place ?x)
      						(place ?y)
      						(at-ferry ?x))
     						((at-ferry ?y)
      						(not (at-ferry ?x)))))
			NIL)
		(equal
			 (is-operator? '(sail (?x ?y)
     						(place ?x
      						place ?y
      						at-ferry ?x)
     						((at-ferry ?y)
      						(not (at-ferry ?x)))))
			NIL)
		(equal
			 (is-operator? '(sail (?x ?y)
     						((place ?x)
      						(place ?y)
      						(at-ferry ?x))
     						(at-ferry ?y
      						(not (at-ferry ?x)))))
			NIL)
		(equal
			 (is-operator? '(some (trash written) (?x ?y) example))
			NIL))
	(format t "~%"))

(deftest test-literal-unifiers ()
	(format t "Test function for literal-unifiers.~%Cases 1-6 are ~
		from email with assignment clarifications.~%Cases 7-14 are ~
		about bad inputs or nil(s) in inputs.~%")
	(check
		(equal 
			(literal-unifiers 
				'(not (on ?y ?z)) 
				'((on A B) (on B Table) (clear A)) 
				'(A B Table))
			'(((?Y A) (?Z TABLE)) 
			  ((?Y B) (?Z A)) 
			  ((?Y TABLE) (?Z A)) 
			  ((?Y TABLE) (?Z B))))
		(equal
			(literal-unifiers 
				'(on A B) 
				'((on A B) (on B Table) (clear X)) 
				'(A B Table))
			t)
		(equal
			(literal-unifiers 
				'(on C B) 
				'((on A B) (on B Table) (clear X)) 
				'(A B Table))
			nil)
		(equal
			(literal-unifiers 
				'(not (on A B)) 
				'((on A B) (on B Table) (clear X)) 
				'(A B Table)) 
			nil)
		(equal
			(literal-unifiers 
				'(not (on C B)) 
				'((on A B) (on B Table) (clear X)) 
				'(A B Table)) 
			t)
		(equal
			(literal-unifiers 
				'(on ?y ?z) 
				'((on A B) (on B Table) (clear A)) 
				'(A B Table)) 
			'(((?y A) (?z B)) ((?y B) (?z Table))))
		(equal
			(literal-unifiers 
				'(on y ?z) 
				'((on A B) (on B Table) (clear A)) 
				'(A B Table)) 
			nil)
		(equal
			(literal-unifiers 
				'(on ?y ?z) 
				'((on ?A B) (on B Table) (clear A)) 
				'(A B Table)) 
			nil)
		(equal
			(literal-unifiers 
				'(on ?y ?z) 
				'((on A B) (on B Table) (clear A)) 
				'(A ?B Table)) 
			nil)
		(equal
			(literal-unifiers 
				'(on) 
				'((on A B) (on B Table) (clear A)) 
				'(A B Table)) 
			nil)
		(equal
			(literal-unifiers 
				nil
				'((on A B) (on B Table) (clear A)) 
				'(A B Table)) 
			nil)
		(equal
			(literal-unifiers 
				'(on ?y ?z) 
				nil 
				'(A B Table)) 
			nil)
		(equal
			(literal-unifiers 
				'(on ?y ?z) 
				'((on A B) (on B Table) (clear A)) 
				nil) 
			nil)
		(equal
			(literal-unifiers 
				'(on ?y ?z) 
				'(not (a (correct (states) list) probably)) 
				'(A B Table)) 
			nil)
		)
	(format t "~%"))

(deftest test-operator-instances()
	(format t "Function to test operator-instances.~%Case #1 is from ~
		assignment examples.~%Cases #2-6 are about bad input or nil(s)~
		in the input.~%Cases #7-12 have operators from email with ~
		domains and some imagined state.~%")
	(check
		(equal
			(operator-instances
				'(MOVE-TO-TABLE (?x ?y)
					((on ?x ?y) (clear ?x))
					((on ?x Table) (clear ?y) (not (on ?x ?y))))
				'((on A B) (on B Table) (clear A)))
			'(((?y B) (?x A))))
		(equal
			(operator-instances
				'(MOVE-TO-TABLE (x ?y)
					((on ?x ?y) (clear ?x))
					((on ?x Table) (clear ?y) (not (on ?x ?y))))
				'((on A B) (on B Table) (clear A)))
			NIL)
		(equal
			(operator-instances
				'(MOVE-TO-TABLE (?x ?y)
					(on ?x ?y clear ?x)
					((on ?x Table) (clear ?y) (not (on ?x ?y))))
				'((on A B) (on B Table) (clear A)))
			NIL)
		(equal
			(operator-instances
				'(MOVE-TO-TABLE ?x ?y
					((on ?x ?y) (clear ?x))
					((on ?x Table) (clear ?y) (not (on ?x ?y))))
				'((on A B) (on B Table) (clear A)))
			NIL)
		(equal
			(operator-instances
				'(MOVE-TO-TABLE (?x ?y)
					((on ?x ?y) (clear ?x))
					((on ?x Table) (clear ?y) (not (on ?x ?y))))
				'())
			NIL)
		(equal
			(operator-instances
				NIL
				'((on A B) (on B Table) (clear A)))
			NIL)
		(equal
			(operator-instances
				'(MOVE-TO-TABLE (?x ?y)
					((on ?x ?y) (clear ?x))
					((on ?x Table) (clear ?y) (not (on ?x ?y))))
				'((on A B) (on B Table)))
			NIL)
		(equal
			(operator-instances
				'(MOVE-TO-TABLE (?x ?y)
					((on ?x ?y) (clear ?x))
					((on ?x Table) (clear ?y) (not (on ?x ?y))))
				'((on A B) (on B C) (on C D) (on D Table) (clear A)))
			'(((?y B) (?x A))))
		(equal
			(operator-instances
				'(go-to (?x ?y)
    			  ((location ?x) (location ?y) (on-floor) (at Monkey ?y))
     			  ((at Monkey ?x) (not (at Monkey ?y))))
				'((location A) (location B) (on-floor) (at Monkey B)))
			'(((?x A) (?y B))))
		(equal
			(operator-instances
				'(go-to (?x ?y)
    			  ((location ?x) (location ?y) (on-floor) (at Monkey ?y))
     			  ((at Monkey ?x) (not (at Monkey ?y))))
				'((location A) (location B) (at Monkey B)))
			NIL)
		(equal
			(operator-instances
				'(go-to (?x ?y)
    			  ((location ?x) (location ?y) (on-floor) (at Monkey ?y))
     			  ((at Monkey ?x) (not (at Monkey ?y))))
				'((location A) (location B) (on-floor) (at Monkey A)))
			'(((?x B) (?y A))))
		(equal
			(operator-instances
				'(go-to (?x ?y)
    			  ((location ?x) (location ?y) (on-floor) (at Monkey ?y))
     			  ((at Monkey ?x) (not (at Monkey ?y))))
				'((location A) (location B) (on-floor) (at Monkey C)))
			NIL)
		)
	(format t "~%"))

(deftest test-lisp2 ()
  (format t "This program runs all the test cases for all the ~
    functions from the Lisp Problem Set 2 and some additional ~
    functions used in the main ones.~%
    Author: Andrii Osipa~%Test framework:Peter Seibel \"~
    Practical Common Lisp\", Chapter 9~%~%")
  (combine-results
    (test-distinct-bindings)
    (test-literal-unifiers)
    (test-operator-instances)
    (test-is-state-lst?)
    (test-is-operator?)
    ))
