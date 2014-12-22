(load "2.3.2.scm")

(define (augend s)
  (if (null? (cdddr s)) (caddr s)
    (cons '+ (cddr s))
  )
)

(define (multiplicand p)
  (if (null? (cdddr p)) (caddr p)
    (cons '* (cddr p))
  )
)

; test
(display (deriv '(* x y (+ x 3 y)) 'x))(newline)
(display (deriv '(+ x (* 1 (+ x x) y)) 'x))(newline)
