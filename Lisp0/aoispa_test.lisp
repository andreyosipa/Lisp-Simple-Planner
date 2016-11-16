(deftest test-count-occur ()
	(check
		(= (count-occur NIL '(a b c)) 0)
		(= (count-occur 'a ()) 0)
		(= (count-occur NIL ()) 0)
		(= (count-occur 'a '(a a a a a a)) 6)
		(= (count-occur 'a '(b c d g a f)) 1)
		(= (count-occur 'a '(d b c)) 0)
		(= (count-occur 'a '((a a a) (a a a))) 6)
		(= (count-occur 'a '(a b (c (d a) g))) 2)
		(= (count-occur 'a '(((((d) f) g) l) k)) 0)))