(load "1.21.scm")
(define (prime? n)
  (= (smallest-divisor n) n)
)

(define (next-odd n)
  (if (even? n) (+ n 1) (+ n 2))
)

(define (search-prime n)
  (if (prime? n) n (search-prime (next-odd n)))
)

(define (time-search-prime n)
  (let ((start-time (real-time-clock)))
    (newline)(display (search-prime (next-odd n)))(newline)
    (- (real-time-clock) start-time)
  )
)

; (time-search-prime 100000000)
