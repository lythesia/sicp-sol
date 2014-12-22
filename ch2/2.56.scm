(load "2.3.2.scm")
(load "../ch1/1.16.scm") ; for fast-expt

(define (make-exponentiation b e)
  (cond
    ((=number? b 0) 0)
    ((=number? e 0) 1)
    ((=number? b 1) 1)
    ((=number? e 1) b)
    ((and (number? b) (number? e)) (fast-expt b e))
    (else (list '^ b e))
  )
)
(define (exponentiation? e)
  (and (pair? e) (eq? '^ (car e)))
)
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (deriv exp var)
  (cond
    ; dc/dx
    ((number? exp) 0)

    ; dx/dx (dy/dx)
    ((variable? exp)
     (if (same-variable? exp var) 1 0)
    )

    ; d(u+v)/dx
    ((sum? exp)
     (make-sum (deriv (addend exp) var) (deriv (augend exp) var))
    )

    ; d(uv)/dx
    ((product? exp)
     (make-sum
       (make-product (multiplier exp) (deriv (multiplicand exp) var))
       (make-product (multiplicand exp) (deriv (multiplier exp) var))
     )
    )

    ; d(u^n)/dx
    ((exponentiation? exp)
     (let ((b (base exp)) (e (exponent exp)))
       (make-product
         (make-product e (make-exponentiation b (1- e)))
         (deriv b var)
       )
     )
    )

    (else (error "unknown expression type -- DERIV" exp))
  )
)

; test
; (display (deriv '(+ (+ x 2) (* (^ x 5) y)) 'x))(newline)
