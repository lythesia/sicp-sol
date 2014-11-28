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

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (cubic a b c)
  (newton-method (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)) 1.0)
)

; (display (cubic 3 2 1))(newline)
; (display (cubic 2 5 5))(newline)
