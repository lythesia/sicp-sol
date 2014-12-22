; symbols -> expression

(define (variable? x) (symbol? x))
(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))
(define (=number? sym num) (and (number? sym) (= sym num))) ; = for numerical equality

(define (make-sum x y) 
  (cond
    ((=number? x 0) y)
    ((=number? y 0) x)
    ((and (number? x) (number? y)) (+ x y))
    (else (list '+ x y))
  )
)
(define (sum? s) (and (pair? s) (eq? (car s) '+))) ; sum-form iff start with +
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product x y) 
  (cond
    ((=number? x 0) 0)
    ((=number? y 0) 0)
    ((=number? x 1) y)
    ((=number? y 1) x)
    ((and (number? x) (number? y)) (* x y))
    (else (list '* x y))
  )
)
(define (product? p) (and (pair? p) (eq? (car p) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

; deriv for expression in form of symbols
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

    (else (error "unknown expression type -- DERIV" exp))
  )
)

; test
; (display (deriv '(+ x 3) 'x))(newline)
; (display (deriv '(* (* x y) (+ x 3)) 'x))(newline)
