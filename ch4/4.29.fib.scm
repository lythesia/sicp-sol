(define (fib n)
  (if (<= n 2)
    1
    (+ (fib (- n 1)) (fib (- n 2)))
  )
)
(define (test x)
  (define (iter t)
    (if (= t 0)
      0
      (+ x (iter (- t 1)))
    )
  )
  (iter 10)
)

;; fib itself is not memorized, but `x` in test is memorized!
(test (fib 15))
(exit)
