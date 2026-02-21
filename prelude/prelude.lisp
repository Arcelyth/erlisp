(val newline (itoc 10))

(define println (s)
  (let ((ok (print s)))
    (print newline)))
