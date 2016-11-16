"Test Framework from Peter Siebel \"Practical Common Lisp\""
(defvar *test-name* nil)

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
    ,@body))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call other
test functions or use `check' to run individual test cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in `forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating `forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by `check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

"Tests."

(deftest test-count-occur ()
	(format t "Test for function count-occur.~%Cases #1-2 tests are ~
		about incorrect input.~%Tests #3-5 are cases with NIL(s) in ~
		input.~%Cases #6-8 are about flat lists.~%Cases #9-11 are ~
		about complex lists with lists inside.~%")
	(check
		(equal (count-occur '(a b) '(a b c)) "Error: s is not an atom.")
		(equal (count-occur 'a 'b) "Error: lst is not a list.")
		(= (count-occur NIL '(a b c)) 0)
		(= (count-occur 'a ()) 0)
		(= (count-occur NIL ()) 0)
		(= (count-occur 'a '(a a a a a a)) 6)
		(= (count-occur 'a '(b c d g a f)) 1)
		(= (count-occur 'a '(d b c)) 0)
		(= (count-occur 'a '((a a a) (a a a))) 6)
		(= (count-occur 'a '(a b (c (d a) g))) 2)
		(= (count-occur 'a '(((((d) f) g) l) k)) 0))
	(format t "~%"))

(deftest test-subexpr ()
	(format t "Test for function subexpr.~%Cases #1-5 are some of ~
		the cases from examples.~%Cases #6-8 have NIL(s) in the ~
		input.~%Case #9 have equal lists(difference from case #1) ~
		as input.~%")
	(check
		(equal (subexpr 'a 'a) t)
		(equal (subexpr 
					'|(OP.CIT.)| 
					'(Minsky \(op\.cit\.\))) 
			'(|(OP.CIT.)|))
		(equal (subexpr 
				'(a b c) 
				'(d (a b c) (e f g))) 
			'((a b c) (e f g)))
		(equal (subexpr 'b '(d (a b c))) '(b c))
		(equal (subexpr '(a b c) '(a b c (a b c))) '((a b c)))
		(equal (subexpr () ()) t)
		(equal (subexpr () '(a b c)) NIL)
		(equal (subexpr '(a b c) ()) NIL)
		(equal (subexpr '(a b c) '(a b c)) t))
		(format t "~%"))

(deftest test-my-flatten ()
	(format t "Test for my-flatten function.~%Case #1 is about ~
		empty input.~%Cases #2-3 are about already flat lists.~%~
		Cases #4-6 have complex lists in the input.~%Case #7 about~
		non-list input.~%")
	(check
		(equal (my-flatten ()) ())
		(equal (my-flatten '(a)) '(a))
		(equal (my-flatten '(a b c d)) '(a b c d))
		(equal (my-flatten '(a b (c d))) '(a b c d))
		(equal (my-flatten '((a b) (c d))) '(a b c d))
		(equal (my-flatten '(a (b (c (d))))) '(a b c d)))
		(equal (my-flatten 'a) "lst is not a list")
	(format t "~%"))

(deftest test-my-intersection ()
	(format t "Test for my-intersection.~%Cases #1-3 test errors ~
		with wrong input - not lists.~%Cases #4-5 have NIL(s) in the ~
		input.~%Case #6 have flat lists in the input.~%Case #7 has ~
		lists with complex structure but has non-empty ~
		intersection.~%Cases #8-10 have complex lists with same ~
		elements but not always on top level, so intersection by ~
		this definition is empty there.~%")
	(check
		(equal 
			(my-intersection 'a '(a b c)) 
			"Error, not all input arguments are lists.")
		(equal 
			(my-intersection 'a 'b) 
			"Error, not all input arguments are lists.")
		(equal 
			(my-intersection '(a b c) 'a) 
			"Error, not all input arguments are lists.")
		(equal (my-intersection () ()) ())
		(equal (my-intersection () '(a b)) ())
		(equal (my-intersection '(a) '(a b)) '(a))
		(equal (my-intersection 
					'(a (a b) (c (d)) (e a)) 
					'(b (a b) (c (d)) a)) 
			'(a (a b) (c (d)))) 
		(equal (my-intersection 
					'(a (a b) (c (d)) (e a)) 
					'(b ((a b) c a))) 
			())
		(equal (my-intersection 
					'(a (b (c (d (e f g) f (k l)) m)))
					'(f (k l))) 
			())
		(equal (my-intersection '((a b c) d) '(a b c)) ()))
	(format t "~%"))

(deftest test-fib()
	(format t "Tests for fib function.~%Cases #1-2 handle wrong input.~
		~%Cases #3-11 have different n and compare output with real~
		value of the elements if Fibonacci sequence.~%")
	(check
		(equal (fib 1.45) "Error: n < 0 or n is not an integer")
		(equal (fib -1) "Error: n < 0 or n is not an integer")
		(= (fib 0) 1)
		(= (fib 1) 1)
		(= (fib 2) 2)
		(= (fib 3) 3)
		(= (fib 4) 5)
		(= (fib 5) 8)
		(= (fib 6) 13)
		(= (fib 20) 10946)
		(= (fib 37) 39088169))
	(format t "	~%"))

(deftest test-merge-occurrence-counts ()
	(format t "Test for merge-occurrence-counts function.~%Cases #1-3 ~
		has empty input list(s).~%Case #4 has same entities names ~
		in both lists but with different counts and order.~%Cases ~
		#5-6 have lists with intersection in entities names, but one ~
		of lists in each case has entity name which is not in the ~
		other list's entities names.~%Case #7 have lists, that each ~
		of them have entity name which is not on the other list.~%~
		Case #8 have lists with no intersection in entities names.~%")
	(check
		(equal (merge-occurrence-counts () ()) ())
		(equal 
			(merge-occurrence-counts () '((1 cat) (1 dog))) 
			'((1 cat) (1 dog)))
		(equal (merge-occurrence-counts '((1 cat) (1 dog)) ()) 
			'((1 cat) (1 dog)))
		(equal 
			(merge-occurrence-counts 
				'((1 cat) (4 dog) (5 student) (2 duck)) 
				'((1 dog) (8 duck) (9 cat) (444 student)))
			'((10 cat) (5 dog) (449 student) (10 duck)))
		(equal 
			(merge-occurrence-counts 
				'((1 cat) (1 dog)) 
				'((3 cat)))
			'((4 cat) (1 dog)))
		(equal 
			(merge-occurrence-counts 
				'((3 cat))
				'((1 cat) (1 dog))) 
			'((4 cat) (1 dog)))
		(equal
			(merge-occurrence-counts
				'((3 cat) (1 dog) (1 human))
				'((1 human) (4 cow)))
			'((3 cat) (1 dog) (2 human) (4 cow)))
		(equal
			(merge-occurrence-counts
				'((9 rabbit) (3 giraffe))
				'((1 human) (6 devil)))
			'((9 rabbit) (3 giraffe) (1 human) (6 devil))))
	(format t "~%"))

(deftest test-lisp0 ()
	(format t "This program runs all the test cases for all the ~
		functions from the Lisp Problem Set 0.~%~
		Author: Andrii Osipa~%Test framework:Peter Seibel \"~
		Practical Common Lisp\", Chapter 9~%~%")
	(combine-results
		(test-count-occur)
		(test-subexpr)
		(test-my-flatten)
		(test-my-intersection)
		(test-fib)
		(test-merge-occurrence-counts)))