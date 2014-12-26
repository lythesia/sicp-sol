(load "util.scm") ; for make-table
(load "../ch1/1.16.scm") ; for fast-expt

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; a)
; (operator) cannot apply on number/variable

; b,c)
(define (variable? x) (symbol? x))
(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))
(define (=number? sym num) (and (number? sym) (= sym num))) ; = for numerical equality

(define (install-deriv-package)
  ;; sum
  (define (make-sum x y)
    (cond
      ((=number? x 0) y)
      ((=number? y 0) x)
      ((and (number? x) (number? y)) (+ x y))
      (else (list '+ x y))
    )
  )
  (define (addend opds) (car opds))
  (define (augend opds) (cadr opds))
  (define (deriv-sum opds var)
    (make-sum
      (deriv (addend opds) var)
      (deriv (augend opds) var)
    )
  )

  ;; product
  (define (make-product x y)
    (cond
      ((or (=number? x 0) (=number? y 0)) 0)
      ((=number? x 1) y)
      ((=number? y 1) x)
      ((and (number? x) (number? y)) (* x y))
      (else (list '* x y))
    )
  )
  (define (multiplier opds) (car opds))
  (define (multicand opds) (cadr opds))
  (define (deriv-product opds var)
    (let ((mr (multiplier opds)) (md (multicand opds)))
      (make-sum
        (make-product mr (deriv md var))
        (make-product md (deriv mr var))
      )
    )
  )

  ;; exponent
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
  (define (base opds) (car opds))
  (define (exponent opds) (cadr opds))
  (define (deriv-exponentiation opds var)
    (let ((b (base opds)) (e (exponent opds)))
     (make-product
       (make-product e (make-exponentiation b (1- e)))
       (deriv b var)
     )
    )
  )

  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '^ deriv-exponentiation)
  'done
)

(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    (else ((get 'deriv (operator exp)) (operands exp) var))
  )
)

; in-order
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; d)
; s/put (x) (y) (z)/put \2 \1 \3/g

; test
; (install-deriv-package)
; (display (deriv '(+ 3 x) 'x))(newline)
; (display (deriv '(* (* x y) (+ x 3)) 'x))(newline)
; (display (deriv '(+ (+ x 2) (* (^ x 5) y)) 'x))(newline)
