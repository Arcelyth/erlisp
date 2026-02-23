(val newline (itoc 10))
(val space (itoc 32))

(define println (s) 
  (begin 
    (print s)
    (print newline)
    'ok ))

(val cons pair)

(define getline ()
  (let* ((ic (getchar))
         (c (itoc ic)))
    (if (or (eq c newline) (eq ic ~1))
      empty-symbol
      (cat c (getline)))))

(define null? (xs)
  (eq xs '()))

(define length (ls)
  (if (null? ls)
    0
    (+ 1 (length (cdr ls)))))

(define take (n ls)
  (if (or (< n 1) (null? ls))
    '()
    (cons (car ls) (take (- n 1) (cdr ls)))))

(define drop (n ls)
  (if (or (< n 1) (null? ls))
    ls
    (drop (- n 1) (cdr ls))))

(define merge (xs ys)
  (if (null? xs)
    ys
    (if (null? ys)
      xs
      (if (< (car xs) (car ys))
        (cons (car xs) (merge (cdr xs) ys))
        (cons (car ys) (merge xs (cdr ys)))))))

(define mergesort (ls)
  (if (null? ls)
    ls
    (if (null? (cdr ls))
      ls
      (let* ((size (length ls))
             (half (/ size 2))
             (first (take half ls))
             (second (drop half ls)))
        (merge (mergesort first) (mergesort second))))))
