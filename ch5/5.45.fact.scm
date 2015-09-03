(define (factorial n)
  (if (= n 1) 1 (* (factorial (- n 1)) n))
)

(factorial 5)
(factorial 10)
(factorial 25)
(factorial 50)
(factorial 100)
(factorial 250)
(factorial 500)
(factorial 1000)