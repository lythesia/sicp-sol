(load "util.scm")

(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next-guess (improve guess)))
        (if (good-enough? guess next-guess) next-guess
          (try next-guess))
      )
    )
    (try first-guess)
  )
)

(define (fixed-point f first-guess)
  (define (close-enough? x y) (< (abs (- x y)) eps))
  (define (improve guess) (f guess))
  ((iterative-improve close-enough? improve) first-guess)
)

; (display (fixed-point cos 1.0))(newline)

(define (sqrt x)
  (define (close-enough? x y) (< (abs (- x y)) eps))
  (define (improve guess) (average guess (/ x guess )))
  ((iterative-improve close-enough? improve) 1.0)
)

(display (sqrt 9.0))(newline)
