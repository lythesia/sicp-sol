;; 5.04.scm < this
(define (fib n)
  (if (<= n 2)
    1
    (+ (fib (- n 1)) (fib (- n 2)))
  )
)
(display (fib 10)) ; higher is slow..
(exit)
