(load "1.36.scm")

(define dx 0.001)
(define eps 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x))))
)

(define (newton-method g x)
  (fixed-point (newton-transform g) x eps)
)

(define (sqr x) (* x x))
(define (cube x) (* x x x))

; 1/2(fx + x) is a special case of newton for sqrt
(define (sqrt x)
  (newton-method (lambda (y) (- (cube y) x)) 1.0)
)

; (display (sqrt 27.0))(newline)
