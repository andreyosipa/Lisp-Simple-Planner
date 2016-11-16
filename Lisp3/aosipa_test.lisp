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

(deftest test-store-transition ()
  (format t "Tests for store-transition function.~%Cases #1-4: bad or ~
    NIL input.~%Cases #5,7: adding new state.~%Case #6: adding ~
    existing state.~%")
  (init_hashtable)
  (check
    (equal (store-transition NIL NIL NIL) NIL)
    (equal (store-transition '(not a state) NIL NIL) NIL)
    (equal (store-transition NIL 'a NIL) NIL)
    (equal (store-transition nil nil '(not a state)) NIL)
    (equal 
      (store-transition '((on a b)) '(move a c) '((on a c))) 
      '((on a c)))
    (equal 
      (store-transition '((on a b)) '(move a c) '((on a c))) 
      NIL)
    (equal 
      (store-transition '((on a b)) '(move a c) '((on v c))) 
      '((on v c))))
  (format t "~%"))

(deftest test-successor-state()
  (format t "Tests for successor-state function.~%Cases #1-2: bad or ~
    NIL input.~%Cases #3: adding new state, positive case.~%Case #4: ~
    adding existing state.~%Case #5: bad unifier.~%Case #6: adding ~
    new state, positive case.~%")
  (init_hashtable)
  (check
    (equal (successor-state nil nil nil) nil)
    (equal (successor-state nil '(not an operator) '((?x a))) nil)
    (equal 
      (successor-state
        '((auto a)
          (auto b)
          (place c)
          (place d)
          (at a c)
          (at-ferry c)
          (empty-ferry))
        '(board (?x ?y)
                 ((auto ?x)
                 (place ?y)
                 (at ?x ?y)
                   (at-ferry ?y)
                   (empty-ferry))
                 ((on ?x Ferry)
                 (not (at ?x ?y))
                   (not (empty-ferry))))
        '((?x a) (?y c)))
        '((auto A) (auto B) (place C) (place D) (at-ferry C) (on A ferry)))
    (equal 
      (successor-state
        '((auto a) (auto b) (place c) (place d) (at a c) (at-ferry c) (empty-ferry))
        '(board (?x ?y)
                 ((auto ?x)
                 (place ?y)
                 (at ?x ?y)
                   (at-ferry ?y)
                   (empty-ferry))
                 ((on ?x Ferry)
                 (not (at ?x ?y))
                   (not (empty-ferry))))
        '((?x a) (?y c)))
        NIL)
    (equal
      (successor-state
        '((place c) (place d) (at-ferry c) (auto a) (auto b))
        '(sail (?x ?y)
               ((place ?x)
               (place ?y)
               (at-ferry ?x))
               ((at-ferry ?y)
               (not (at-ferry ?x))))
        '((?x a)))
      NIL)
    (equal
      (successor-state
        '((place c) (place d) (at-ferry c) (auto a) (auto b))
        '(sail (?x ?y)
               ((place ?x)
               (place ?y)
               (at-ferry ?x))
               ((at-ferry ?y)
               (not (at-ferry ?x))))
        '((?x c) (?y d)))
      '((place C) (place D) (auto A) (auto B) (at-ferry D)))
    )
  (format t "~%"))

(deftest test-successor-states()
  (format t "Tests for successor-states function.~%Cases #1-3: bad or ~
    NIL input.~%Case #4: positive case with new states.~%Case #5: ~
    positive case with nly one new state from only one operator.~%")
  (init_hashtable)
  (check
    (equal (successor-states nil nil) nil)
    (equal (successor-states 
      '(not a state list) 
      '((sail (?x ?y)
               ((place ?x)
               (place ?y)
               (at-ferry ?x))
               ((at-ferry ?y)
               (not (at-ferry ?x))))))
      NIL)
    (equal (successor-states
      '((place c) (place d) (at-ferry c) (auto a) (auto b))
      '(not an operator))
      NIL)
    (equal (successor-states
      '((auto a) (auto b) (place c) (place d) (at a c) (at-ferry c) (empty-ferry))
      '((sail (?x ?y)
               ((place ?x)
               (place ?y)
               (at-ferry ?x))
               ((at-ferry ?y)
               (not (at-ferry ?x))))
        (board (?x ?y)
                ((auto ?x)
                 (place ?y)
                 (at ?x ?y)
                 (at-ferry ?y)
                 (empty-ferry))
                ((on ?x Ferry)
                 (not (at ?x ?y))
                 (not (empty-ferry))))))
      '(((auto A) (auto B) (place C) (place D) (at A C) (empty-ferry) 
        (at-ferry D)) ((auto A) (auto B) (place C) (place D) 
        (at-ferry C) (on A ferry))))
    (equal (successor-states
      '((auto a) (auto b) (place c) (place d) (at a c) (at-ferry d) (empty-ferry))
      '((sail (?x ?y)
               ((place ?x)
               (place ?y)
               (at-ferry ?x))
               ((at-ferry ?y)
               (not (at-ferry ?x))))
        (board (?x ?y)
                ((auto ?x)
                 (place ?y)
                 (at ?x ?y)
                 (at-ferry ?y)
                 (empty-ferry))
                ((on ?x Ferry)
                 (not (at ?x ?y))
                 (not (empty-ferry))))))
      '(((auto A) (auto B) (place C) (place D) (at A C) (empty-ferry) (at-ferry C))))
    
    )
  (format t "~%"))

(deftest test-check-goal ()
  (format t "Tests for successor-states function.~%Cases #1-3: bad or ~
    NIL input.~%Cases #4-8: positive cases.~%")
  (check
    (equal (check-goal nil nil) T)
    (equal (check-goal '(not a state) nil) NIL)
    (equal (check-goal nil '(not a goal)) NIL)
    (equal (check-goal '((on a b)) nil) T)
    (equal (check-goal '((on a b)) '((on a b))) T)
    (equal (check-goal '((on a b)) '((on c b))) NIL)
    (equal (check-goal '((on a b) (on c b)) '((on a b))) T)
    (equal (check-goal '((on a b) (on c b) (on d e) (on m k)) '((on c b) (on m k))) T)
  )
  (format t "~%"))

(deftest test-find-plan ()
  (format t "Tests for find-plan function.~%Cases #1-3: NIL/bad input.~
    ~%Case #2-4: positive cases with achievable and not-achievable goals.~%")
  (check
    (equal (find-plan nil nil nil) nil)
    (equal (find-plan '(not an op) nil nil) nil)
    (equal (find-plan nil nil nil) nil)
    (equal (find-plan '((move (?x ?y) NIL NIL))'((at a b)) '((at a b))) T)
    (equal (find-plan  
                '((move (?x ?y) 
                        ((block ?x) (block ?y) (free ?x) (free ?y))
                        ((on ?x ?y) (not (free ?y)))))
                '((on a b) (block a) (block b) (block c) (free c) (free a))
                '((on a c)))
      '((move a c)))
    (equal (find-plan  
                '((move (?x ?y) 
                        ((block ?x) (block ?y) (free ?x) (free ?y))
                        ((on ?x ?y) (not (free ?y)))))
                '((on a b) (block a) (block b) (block c) (free c) (free a))
                '((on c a)))
      '((move c a)))
    (equal (find-plan  
                '((move (?x ?y) 
                        ((block ?x) (block ?y) (free ?x) (free ?y))
                        ((on ?x ?y) (not (free ?y)))))
                '((on a b) (block a) (block b) (block c) (free c) (free a))
                '((on c k)))
      NIL))
  (format t "~%"))

(deftest test-lisp3 ()
  (format t "This program runs all the test cases for all the ~
    functions from the Lisp Problem Set 3.~%
    Author: Andrii Osipa~%Test framework:Peter Seibel \"~
    Practical Common Lisp\", Chapter 9~%~%")
  (combine-results
    (test-store-transition)
    (test-successor-state)
    (test-successor-states)
    (test-check-goal)
    (test-find-plan)
    ))