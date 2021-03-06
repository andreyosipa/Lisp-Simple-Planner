(defstruct hashtable_entry
	(prev NIL :type list)
	(next NIL :type list))

(defparameter states_table (make-hash-table :test #'equal))

(defun init_hashtable()
	;function to reinitiate hashtable as it contains already records 
	;that may cause problems for solving problems with same 
	;operators/states/etc.
	(defparameter states_table (make-hash-table :test #'equal)))

(defun sort-state(state)
	;;Input: state list.
	;;Output: state as ordered list of string.
	(sort 
		(mapcar 
			(lambda (x) 
				(subseq 
					(write-to-string x) 
					1 
					(- (length (write-to-string x)) 1))) 
			state) 
		#'string<))

(defun store-transition(state action new_state)
	;;Input: state 	   - list of ground predicates(will not reject list 
	;;					 with not only ground predicates).
	;;		 action    - list that represents operator application to 
	;;					 some constants.
	;;		 new_state - list of ground predicates(...)
  	;;Output: new state added to hashtable with states or 
  	;;		  NIL otherwise.
	(cond 
		((and 
			(is-state-lst? state) 
			(is-state-lst? new_state)
			(is-const-lst? action))
		(if 
			(gethash (sort-state new_state) states_table)
			(progn
				(setf 
					(gethash (sort-state new_state) states_table)
					(make-hashtable_entry 
						:prev (append 
								(hashtable_entry-prev 
									(gethash (sort-state new_state) 
										states_table)) 
								(list (list state action)))
						:next (hashtable_entry-next 
									(gethash (sort-state new_state) 
										states_table))))				
				(return-from store-transition NIL))
			(progn
				(setf
					(gethash (sort-state new_state) states_table) 
					(make-hashtable_entry 
						:prev (list (list state action)) 
						:next NIL))
				(return-from store-transition new_state))))
		(T NIL)))

(defun is-negated?(predication)
	(equal (car predication) 'not))

(defun apply-unifier-to-list (unifier predications)
	;function to apply unifier to a list of literals.
	(mapcar 
		(lambda(predication)
			(apply-unifier unifier predication))
			predications))

(defun successor-state(state operator operator_instance)
	;;Input: state - state list.
	;;		 operator - list with 4 elements defined in is-operator? 
	;;					function.
	;;		 operator_instance - unifier for operator's variables.
	;;Output: new state added to hashtable with states.
	(cond
		((and 
			(is-state-lst? state)
			(is-operator? operator)
			(is-unifier? operator_instance)
			(not (has-variables (apply-unifier 
									operator_instance 
									(nth 1 operator)))))
			(store-transition 
				;state
				state
				;action
				(apply-unifier 
					operator_instance 
					;0th element of operator is just name and 1st is 
					;variables
					(append (list (nth 0 operator)) (nth 1 operator)))
				;new state
				(remove-if
					#'null
					(remove-duplicates
						(mapcar
							(lambda(predication)
								(cond
									;Case: predication in states and
									;negated in effects.
									((and
										(contains predication state)
										(contains 
											(list 'not predication) 
											(apply-unifier-to-list 
												operator_instance 
												(nth 3 operator))))
									NIL)
									;Case: predication in effects and
									; it is positive and not in state.
									((and
										(contains predication 
											(apply-unifier-to-list 
												operator_instance 
												(nth 3 operator)))
										(not (is-negated? predication))
										(not (contains predication state)))
										predication)
									;Case: other predications in state.
									((contains predication state) predication)
									(T NIL)))
							(append state (apply-unifier-to-list 
											operator_instance 
											(nth 3 operator))))
						:test #'equal))))
		(T NIL)))

(defun successor-states(state operators)
	;;Input: state - state list.
	;;		 operators - list of operators(list with 4 elements 
	;;					 defined in is-operator? function).
	;;Output: new states added to hashtable with states.
	(cond 
		((and 
			(is-state-lst? state)
			(not (null (remove-if-not #'listp operators))) 
			(equal 
				operators
				(remove-if-not #'is-operator? operators)))
			(delete-if 
				#'null
				(mapcan 
					(lambda(operator)
						(delete-if
							#'null
							(mapcar
								(lambda(operator_instance)
									(successor-state 
										state 
										operator 
										operator_instance))
								(operator-instances operator state))))
					operators)))
		(T NIL)))

(defun check-goal(state goal)
	;Input: state.
	;Output: T if goal is a subset of state and NIL otherwise.
	(cond
		((and (is-state-lst? state) (null goal)) T)
		((and (is-state-lst? state) (is-state-lst? goal))
			(null
				(remove-if-not #'null
					(mapcar
						(lambda(predication)
							(contains predication state))
						goal))
				))
		(T NIL)))

(defun contains (item list)
	;Function to check if one list is an element of other.
	(member 
		T
		(mapcar
			(lambda(x)
				(equal x item))
			list)))

(defun states-equal(state1 state2)
	;Check wether two states contain same predications(order doesn`t matter).
	(equal
		(length
			(remove-if-not
				#'null
				(mapcar 
					(lambda (x)
						(contains x state1))
				state2)))
		0))

(defun find-plan(operators initial_state goal)
	;;Input: operators - list of operators(list with 4 elements 
	;;					 defined in is-operator? function).
	;;		 initial_state - state.
	;;		 goal - state to be achieved.
	;;Output: new states added to hashtable with states.
	(cond
		;Case: good input and goal satisfied by state.
		((and 
			(is-state-lst? initial_state)
			(not (null (remove-if-not #'listp operators)))  
			(equal 
				operators
				(delete-if-not #'is-operator? operators))
			(is-state-lst? goal)
			(check-goal initial_state goal))
			T)
		;Case: good input and goal is not satisfied by state.
		((and 
			(is-state-lst? initial_state) 
			(equal 
				operators
				(delete-if-not #'is-operator? operators))
			(is-state-lst? goal))
		(init_hashtable)
		;;Open List
		(setf open_list (list initial_state))
		(loop for x from 1
			while 
			(and 
				(not (null open_list)) 
				;Condition: no state in open_list that satisfies goal.
				(null (remove-if-not 
							(lambda(state) (check-goal state goal)) 
							open_list)))
			do (setf 
					open_list
					(remove-if 
						#'null
						(mapcan 
							(lambda(open_state)
								(successor-states open_state operators))
							open_list))))
		(if 
			;Checking reason of exit. Case: there is a state that 
			;satisfies goal.
			(remove-if-not 
				(lambda(state) (check-goal state goal)) 
				open_list)
			;track back path
			(progn
				;Taking first state from open_list that satisfies
				;goal to track it back t initial state. 
				(setf 
					final_state 
					(car
						(remove-if-not 
							(lambda(x) (check-goal x goal)) 
							open_list)))
				(action-seq final_state initial_state))
			NIL))
		(T NIL)))

(defun action-seq(final_state initial_state)
	;Sequence of actions to get final state from initial one.
	(cond
		((states-equal final_state initial_state) NIL)
		(T
			(remove-if 
				#'null
				(append 
					;Getting action that led to final state and joining
					;with sequence of actions from initial to before-last
					;state.
					(reverse 
						(list 
							(nth 1 (car (hashtable_entry-prev 
											(gethash (sort-state final_state) 
												states_table))))
						(action-seq 
							(nth 0 (car (hashtable_entry-prev 
											(gethash (sort-state final_state) 
												states_table))))
							initial_state))))))))

;;********************************************************************
;;****************** LISP2 ASSIGNMENT ********************************
;;********************************************************************

;;Type-checking functions of Lisp1 and Lisp0 soultions by Gene Kim.

;;; Flattens list using mapcar.
;;; Assumes that the input is a recursive list.
(defun my-flatten (lst)
  (if (not (listp lst)) 
      ;; Base Case.
      ;; If not a list, just return.
      ;; Since we assume input is good, this will only occur in
      ;; internal recursive cases and will be an atom.
      (list lst)
    ;; Recursive Case.
    ;; Flatten each member then reduce the elemnts into one list.
    (reduce #'append
            (mapcar #'my-flatten lst))))

;; Checks if v is a symbol and a variable ("\?.*" regex pattern).
(defun is-var? (v)
  (and (symbolp v) 
       (> (length (string v)) 0) 
       (equal (subseq (string v) 0 1) "?")))

;; Checks if x is a list of symbols.
(defun symbol-lst? (x)
  (and
    (listp x)
    (find-if #'is-var? x)))

;; Helper function to check unifier input.
;; A unifier is a list, where each element is a 2 length list
;; with the first being a variable and the second being a symbol.
(defun is-unifier? (u)
  (and 
    (listp u)
    (not (find-if 
           #'(lambda (x)
               (not (and (listp x) 
                         (eq (length x) 2)
                         (is-var? (first x)) 
                         (symbolp (second x)))))
           u))))

;; Checks if pl is a positive literal.
;; (i.e. a list of symbols where the first symbol is not a variable)
(defun is-pos-literal? (pl)
  (and
    (listp pl)
    (> (length pl) 0)
    (not (is-var? (first pl)))
    (symbol-lst? pl)))

;; Checks if nl is a negative literal.
(defun is-neg-literal? (nl)
  (and 
    (listp nl)
    (eq (length nl) 2)
    (eq (first nl) 'not)
    (is-pos-literal? (second nl))))

;;Functions from Lisp 1 solution by me.
(defun if-unifier(x) 
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
        (if-unifier (cdr x))))))

(defun ind-unif(x)
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

(defun check-atoms(lst)
  ;;Function to check wether list contains only atoms.
  ;;Input:  list.
  ;;Output: boolean.
  (equal
    (remove-if
      (lambda(x)
        (not (atom x)))
      lst)
    lst))

(defun has-variables(lst)
  ;;Function to check wether list contains variables.
  ;;Input:  list.
  ;;Output: boolean.
  (not
    (equal
      (remove-if
        (lambda(x)
          (equal 
              (subseq (write-to-string x) 0 1) "?"))
        (my-flatten lst))
      (my-flatten lst))))

(defun if-literal(x)
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

(defun extract-constants(lits)
  ;;Input: list of states or premises. (In other words: list of literals 
  ;;       with constants and variables).
  ;;Output: List of all individual constants names or NIL if there 
  ;;        is no individual constants or input is wrong.
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
    (T NIL)))

(defun apply-unifier(unif lit)
  ;;Input:  list of unifiers(list of pairs (?var const)) and list 
  ;;      with correct literal. Correct literal must have form 
  ;;      (fun_name literal/variable/constant+).
  ;;Output: Substitution of variables with corresponding 
  ;;      constants or NIL if input is wrong or NIL.
  (labels
    (
      (magic(x y)
        ;;Function that takes unifier (?name value) and part of
        ;;literal and if this part of literal is ?name then
        ;;returns value, else returns same part.
        (if (equal (car y) x)  (second y) x)))
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

(defun unifier(lst1 lst2)
  ;;Input:  two lists of equal length, where first contains only
  ;;      constants or variables and second constants only.
  ;;Output: list of pairs (?variable constant) or NIL if input is
  ;;      wrong or NIL.
  (labels 
    ((my-check(lst)
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
          (T NIL))))
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

;;Lisp Assignment #2
(defun if-neg-literal? (literal)
  (if (if-literal literal) (equal (car literal) 'not)))

(defun if-pos-literal? (literal)
  (not (if-neg-literal? literal)))

(defun is-list-of-variables? (vars)
  ;;Input: list.
  ;;Output: T if giver list is correct list of variables or NIL otherwise.
  (if 
    (null vars) 
    t 
    (and 
      (is-var? (car vars)) 
      (is-list-of-variables? (cdr vars)))))

;Check if given list is a list of constants(= list of not variables).
(defun is-const-lst? (lst)
  (and
    (listp lst)
    (equal lst (remove-if (lambda (x) (or (is-var? x) (listp x))) lst))))

;;Function that returns all possible permutations of the list.
(defun all-permutations (list)
  ;;Input: List.
  ;;Output: List of lists.
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element list)))))))

;;Function that return T if given list is a correct list of states.
(defun is-state-lst? (lst)
  (cond
    ((not (listp lst)) NIL)
    (t 
      (not 
        (find-if-not 
          (lambda (state) 
            (is-const-lst? state)) 
          lst)))))

;;Function that returns T if given list is a list of preconditions or 
;;effects.
(defun is-cond? (condition)
  (not 
      (find-if-not 
          (lambda(x) (and (if-literal x) (listp x)))  
          condition)))

;;Function that return T if given list is correct operator definition.
(defun is-operator? (op)
  (and
    ;;Operator definition has 4 parts: name variables preconditions
    ;;and effects.
    (listp op)
    (equal (length op) 4) 
    (not (is-var? (nth 0 op)))
    (atom (nth 0 op)) 
    (is-cond? (nth 2 op)) 
    (is-cond? (nth 3 op))
    (not (find-if-not #'is-var? (nth 1 op)))))

;;Function that returns list of variables that are involved in given
;;literal.
(defun get-variables(literals)
  (delete-duplicates
    (mapcan 
      (lambda (x) (if (is-var? x) (list x) NIL)) 
      (my-flatten literals))
    :test #'equal))

(defun distinct-bindings (vars constants)
  ;;Input:  two lists: variables and constants.
  ;;Output: list of all possible variable assignments.
  (cond
    ;;NIL inputs
    ((null vars) NIL)
    ((null constants) NIL)
    ;;bad inputs
    ((not (listp vars)) NIL)
    ((not (listp constants)) NIL)
    ;;in this case any assignment is impossible
    ((> (length vars) (length constants)) NIL)
    ;;there are same variable names, bad input
    ((not (equal vars (delete-duplicates vars))) NIL)
    ;;there are same constants names, bad input
    ((not (equal constants (delete-duplicates constants))) NIL)
    ;;good case
    ((and (is-list-of-variables? vars) (is-const-lst? constants))
        (delete-duplicates
          (loop for consts in (all-permutations constants)
            collect (unifier vars (subseq consts 0 (length vars))))
          :test #'equal))
    (t NIL)))

(defun literal-unifiers(literal state constants)
  ;;Input:  list of literals;
  ;;        list of states;
  ;;        list of constants.
  ;;Output: list of all variable assignments that satisfy literal
  ;;        with current states.
  (cond
    ((and 
      (or (if-pos-literal? literal) (if-neg-literal? literal))
      (and (is-const-lst? constants) (is-state-lst? state)))
      (cond
        ;;Input literal is positive literal.
        ((if-pos-literal? literal) 
          (cond 
            ;;If literal has no variables.
            ((not (has-variables literal)) 
              (literal-in-states literal state))
            ;;Otherwise.
            (t
              (mapcan 
                (lambda(binding) 
                 (if 
                    (literal-in-states
                      (apply-unifier binding literal)
                      state)
                    (list binding))) 
               (distinct-bindings (get-variables literal) constants)))))
        ;;Input literal is negative literal.
        ((if-neg-literal? literal) 
          (cond
            ;;If literal has no variables.
            ((not (has-variables literal)) 
              (literal-in-states literal state))
            ;;Otherwise.
            (t
              (mapcan 
               (lambda(binding) 
                 (if 
                    (literal-in-states
                      (apply-unifier binding literal)
                      state)
                  (list binding))) 
               (distinct-bindings (get-variables (nth 1 literal)) constants)))))))
    (t NIL)))

(defun literal-in-states(literal states)
  (cond
    ((if-pos-literal? literal) 
        (not (null (member literal states :test #'equal))))
     ((if-neg-literal? literal) 
        (or 
          (not (null (member literal states :test #'equal))) 
          (not (null (not (member (nth 1 literal) states :test #'equal))))))))

(defun preconds-instances(preconds state constants)
  ;;Input:  list of preconditions(=literals);
  ;;        list of states;
  ;;        list of constants.
  ;;Output: list of all variable assignments that satisfy literal
  ;;        with current states.
  (cond
    ((and 
        (is-cond? preconds)
        (and 
            (is-state-lst? state)
            (is-const-lst? constants)))
      (remove-if #'null
        (mapcar
          (lambda (binding)
            (if 
              (not 
                (null 
                  (remove-if-not #'null
                    (mapcar
                      (lambda (condition)
                        (literal-in-states (apply-unifier binding condition) state))
                      preconds))))
              NIL
              binding))
            (distinct-bindings (get-variables preconds) constants))))
    (t NIL)))

(defun operator-instances(operator state)
  ;;Input:  operator(list with 4 parts...)
  ;;        states list.
  ;;Output: list of all variable assignments that are possible to run
  ;;        operator on.
  (cond
    ((and (is-operator? operator) (is-state-lst? state)) 
      (preconds-instances 
          (sort 
            (nth 2 operator)
            (lambda(x y)
              (and 
                (or (is-pos-literal? x) (is-neg-literal? x))
                (not (or (is-pos-literal? y) (is-neg-literal? y))))))
          state
          (delete-duplicates
            (append 
              (extract-constants state) 
              (extract-constants (nth 3 operator)))
            :test #'equal)))
    (t NIL)))