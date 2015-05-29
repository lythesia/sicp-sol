(define (try a b)
  (if (= a 0) 1 b)
)
(try 0 (/ 1 0)) ; should 1
(exit)
