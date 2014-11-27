(load "1.37.scm")

(define (tan-cf x k)
  (define (square x) (* x x))
  (define (ni i)
    (if (= i 1) x (- (square x)))
  )
  (define (di i)
    (- (* 2 i) 1)
  )

  (exact->inexact (cont-frac-iter ni di k))
)

(display (tan 10))(newline)
(display (tan-cf 10 100))(newline)
