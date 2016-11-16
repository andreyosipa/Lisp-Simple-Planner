(defun test()
	(load "aosipa_src.lisp")

	(print "count-occur Tests")
	(print "Cases #1-#3 are about NIL in the input")
	(setq s ()))
	(setq lst '(a b c))
	(format "Case#1 s=~A, lst=~A; expected result:0" s lst)
 	(print (equal '0 (count-occur NIL '(a b c))))

	(print "Case#2 s=a, lst=NIL; expected result:0")
	(equal '0 (count-occur 'a ()))

	(print "Case#3 s=NIL, lst=NIL; expected result:0")
	(equal '0 (count-occur NIL ()))

	(print "Cases #4-#6 are about flat lists")
	(print "Case#4 s=a, lst=(a a a a a a); expected result:6")
	(equal '6 (count-occur 'a '(a a a a a a)))

	(print "Case#5 s=a, lst=(b c d a g f); expected result:1")
	(equal '1 (count-occur 'a '(b c d a g f)))

	(print "Case#6 s=a, lst=(c d b); expected result:0")
	(equal '0 (count-occur 'a '(c d b)))

	(print "Cases #7-#9 are about nested lists")
	(print "Case#7 s=a, lst=((a a a) (a a a)); expected result:6")
	(equal '6 (count-occur 'a '((a a a) (a a a))))

	(print "Case#8 s=a, lst=(a b (c (d a) g)); expected result:2")
	(equal '2 (count-occur 'a '(a b (c (d a) g))))

	(print "Case#9 s=a, lst=(((((d) f) g) l) k); expected result:0")
	(equal '0 (count-occur 'a '(((((d) f) g) l) k)))
	)