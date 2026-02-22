(val newline (itoc 10))

(define println (s) 
  (begin 
    (print s)
    (print newline)
    'ok ))
