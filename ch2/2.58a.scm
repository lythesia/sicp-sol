(load "2.3.2.scm")

; (a)
(define (make-sum x y) 
  (cond
    ((=number? x 0) y)
    ((=number? y 0) x)
    ((and (number? x) (number? y)) (+ x y))
    (else (list x '+ y))
  )
)
(define (sum? s) (and (pair? s) (eq? (cadr s) '+))) ; sum-form iff start with +
(define (addend s) (car s))

(define (make-product x y) 
  (cond
    ((=number? x 0) 0)
    ((=number? y 0) 0)
    ((=number? x 1) y)
    ((=number? y 1) x)
    ((and (number? x) (number? y)) (* x y))
    (else (list x '* y))
  )
)
(define (product? p) (and (pair? p) (eq? (cadr p) '*)))
(define (multiplier p) (car p))
