(define (compose f g)
  (lambda (x) (f (g x)))
)

(define (square x) (* x x))

; (display ((compose square 1+) 6))(newline)
