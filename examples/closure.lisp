; erl -noshell -s erlisp main -extra "./examples/closure.lisp" -s init stop
(define adder (x) (+ x 2))
(println (adder 10))

(letrec ((f (lambda (x) (g (+ x 1))))
         (g (lambda (x) (+ x 2))))
    (println (f 0)))

