(define (fib n)
  (fib-iter 1 0 0 1 n)
)

(define (sq n) (* n n))

; faster power with matrix
; http://www.ics.uci.edu/~eppstein/161/960109.html
(define (fib-iter a b p q c)
  (cond ((= c 0) b)
        ((even? c) (fib-iter a b (+ (sq p) (sq q)) (+ (sq q)(* 2 p q)) (/ c 2)))
        (else (fib-iter (+ (* p a) (* q b) (* q a)) (+ (* q a) (* p b)) p q (- c 1)))
  )
)

(display (fib 9))(newline)
