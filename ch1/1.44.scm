(define dx 0.001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))
)

(define (square x) (* x x))
(display ((smooth square) 5))(newline)

(load "1.43.scm")
(define (smooth-n-times f n)
  ((repeated-fast smooth n) f)
)

(display ((smooth-n-times square 10) 5))(newline)
