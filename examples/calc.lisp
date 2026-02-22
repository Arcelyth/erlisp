; erl -noshell -s erlisp main -extra "./examples/calc.lisp" -s init stop
(val pi 3)
(val radius 10)
(val area (* pi (* radius radius)))

(print "The area is: ")
(println area)

