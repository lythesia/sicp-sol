(define x 1)
(define (p (e lazy))
  e ; <- delayed and not exec
  x
)

(p (set! x (cons x '(2))))  ; 1
(exit)
