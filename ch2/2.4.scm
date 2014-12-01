; a lambda which input a func apply on `x` and `y`
(define (cons x y)
  (lambda (m) (m x y))
)

; a lambda which input a `cons` and apply `cons` to a func (p,q -> p)
(define (car z)
  (z (lambda (p q) p))
)

; so..
(define (cdr z)
  (z (lambda (p q) q))
)
