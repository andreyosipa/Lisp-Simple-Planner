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

(deftest test-unifier ()
  (format t "Test for function unifier.~%Cases #1-4 are from task ~
    list.~%Cases #5-6 are about wrong input: vars in second list ~
    and lists in list.~%Cases #7-8 are about wrong input - wrong ~
    length of input.~%Cases #9-10: positive examples.~%")
  (check
    (equal (unifier '(a b c) '(a b c)) T)
    (equal (unifier '(?x b ?y) '(a b c)) '((?x a) (?y c)))
    (equal (unifier '(?x ?x ?y) '(a a c)) '((?x a) (?y c)))
    (equal (unifier '(?x ?x ?y) '(a b c)) NIL)
    (equal (unifier '(?x ?y) '(?x a)) NIL)
    (equal (unifier '(c (a b)) '(5 6)) NIL)
    (equal (unifier '(?x) '(a b)) NIL)
    (equal (unifier '(?x ?y) '(5)) NIL)
    (equal 
        (unifier '(?x ?y ?z ?d) '(a c 5 j)) 
        '((?x a) (?y c) (?z 5) (?d j)))
  )
  (format t "~%"))

(deftest test-apply-unifier ()
  (format t "Test for function apply-unifier.~%Cases #1-4 are from ~
    task list.~%Cases #5-6 are about complex literals.~%Cases #7-8 ~
    are about empty input.~%Cases #9-10 have wrong literal and wrong ~
    unifiers list.~%")
  (check
    (equal (apply-unifier '((?x a) (?y b)) '(on ?x ?y)) '(on a b))
    (equal (apply-unifier '((?x a)) '(on ?x ?y)) '(on a ?y))
    (equal 
        (apply-unifier 
            '((?x a) (?y b) (?z c)) 
            '(on ?x ?y)) 
        '(on a b))
    (equal (apply-unifier '((?x a) (?y b)) '(on ?u ?v)) '(on ?u ?v))
    (equal 
        (apply-unifier 
            '((?x a) (?y b) (?z c)) 
            '(add ?x (add ?y ?z) ?x))
        '(add a (add b c) a))
    (equal 
        (apply-unifier 
            '((?x a) (?y b)) 
            '(not (prop ?x ?y)))
        '(not (prop a b)))
    (equal (apply-unifier '() '(do ?x)) '(do ?x))
    (equal (apply-unifier '((?x 5)) '()) NIL)
    (equal (apply-unifier '((?x 5)) '(?x ?y)) NIL)
    (equal
        (apply-unifier
            '(?x a) 
            '(add ?x ?x))
        NIL))
  (format t "~%"))

(deftest test-extract-constants()
  (format t "Test for extract-constants.~%Case #1: NIL input.~%Cases ~
    #2-3: simple expression without constants and same expression in ~
    list.~%Cases #4-5: simple expressions with consts and vars.~%Cases ~
    #6-8: lists of literals without variables.~%Case #9: list of ~
    complex expressions with variables and constants.~%")
  (check
    (equal (extract-constants NIL) NIL)
    (equal (extract-constants '(on ?x ?y)) NIL)
    (equal (extract-constants '((on ?x ?y))) NIL)
    (equal (extract-constants '(on a b)) '(a b))
    (equal (extract-constants '(on a ?x)) '(a))
    (equal (extract-constants '((on a b))) '(a b))
    (equal (extract-constants '((on a b) (on c d))) '(a b c d))
    (equal (extract-constants '((on a b) (on a d))) '(b a d))
    (equal 
      (extract-constants '((on a b (on c) (at r)) (on v (on ?y)))) 
      '(a b c r v))
  )
  (format t "~%")
)

(deftest test-prune-unifiers()
  (format t "Test for prune-unifiers.~%Case #1: NIL input.~%Cases ~
    #2-3: wrong inputs.~%Case #4: single wrong unifier.~%Case #5: ~
    single good unifier.~%Case #6: list of unifiers with wrong ~
    unifier.~%Case #7: list of good unifiers.~%")
  (check
    (equal (prune-unifiers NIL) NIL)
    (equal (prune-unifiers '(?x a)) NIL)
    (equal (prune-unifiers '(a a)) NIL)
    (equal (prune-unifiers '((?x a) (?y b) (?z a))) NIL)
    (equal (prune-unifiers 
              '((?x a) (?y b) (?z c))) 
              '((?x a) (?y b) (?z c)))
    (equal (prune-unifiers
              '(((?x a) (?y b)) ((?z c) (?x a) (?y c)) ((?s 9))))
              '(((?x a) (?y b)) ((?s 9))))
    (equal (prune-unifiers
              '(((?x a) (?y b)) ((?z c) (?x a) (?y 1)) ((?s 9))))
              '(((?x a) (?y b)) ((?z c) (?x a) (?y 1)) ((?s 9))))
  )
  (format t "~%")
)

(deftest test-lisp1 ()
  (format t "This program runs all the test cases for all the ~
    functions from the Lisp Problem Set 1.~%~
    Author: Andrii Osipa~%Test framework:Peter Seibel \"~
    Practical Common Lisp\", Chapter 9~%~%")
  (combine-results
    (test-unifier)
    (test-apply-unifier)
    (test-extract-constants)
    (test-prune-unifiers)))