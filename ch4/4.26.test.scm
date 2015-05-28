(define (factorial n)
  (unless
    (= n 1)
    (* n (factorial (- n 1)))
    1
  )
)
(factorial 5)
(exit)
