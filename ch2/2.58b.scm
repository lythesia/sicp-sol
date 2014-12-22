(load "2.3.2.scm")
(load "2.2.3.scm") ; for filter

; (b)
(define (prefix-exp exp sep)
  (define (iter result rest)
    (if (or (null? rest) (eq? (car rest) sep))
      result
      (iter (append result (list (car rest))) (cdr rest))
    )
  )
  (iter '() exp)
)

(define (suffix-exp exp sep)
  (cond
    ((null? exp) '())
    ((eq? (car exp) sep) (cdr exp))
    (else (suffix-exp (cdr exp) sep))
  )
)

(define (make-sum x y)
  (cond
    ((=number? x 0) y)
    ((=number? y 0) x)
    ((and (number? x) (number? y)) (+ x y))
    (else
      (let
        ((left-plus
          (if (or (variable? x) (number? x))
            (list x '+)     ; single num/var no need ()
            (append x '(+)) ; term surround with () use append
          )
         )
         (right
           (if (or (variable? y) (number? y))
             (list y)
             y
           )
         )
        )
        (append left-plus right)
      )
    )
  )
)
; if '+' found, it's a sum-form
(define (sum? s)
  (and
    (pair? s)
    (not (null? (filter (lambda (x) (eq? x '+)) s)))
  )
)
(define (addend s)
  (let ((left (prefix-exp s '+)))
    (if (< (length left) 2)
      (car left)  ; ( x ) -> x
      left        ; (complex term)
    )
  )
)
(define (augend s)
  (let ((right (suffix-exp s '+)))
    (if (< (length right) 2)
      (car right)  ; ( x ) -> x
      right        ; (complex term)
    )
  )
)

(define (make-product x y)
  (cond
    ((=number? x 0) 0)
    ((=number? y 0) 0)
    ((=number? x 1) y)
    ((=number? y 1) x)
    ((and (number? x) (number? y)) (* x y))
    (else
      (let
        ((left-mul
           (cond
             ((or (variable? x) (number? x)) (list x '*))
             ; {{ for simply
             ((sum? x) (list (make-sum (addend x) (augend x)) '+))
             ((product? x) (list (make-product (multiplier x) (multiplicand x)) '*))
             ; }}
             (else (append x '(*)))
           )
         )
         (right
           (cond
             ((or (variable? y) (number? y)) (list y))
             ; {{ for simply
             ((sum? y) (list (make-sum (addend y) (augend y)) '+))
             ((product? y) (list (make-product (multiplier y) (multiplicand y)) '*))
             ; }}
             (else y)
           )
         )
        )
        (append left-mul right)
      )
    )
  )
)
; no '+' will be in between product-form
; so middle determines product-form when sum-form is processed before
(define (product? p)
  (and
    (pair? p)
    (eq? (cadr p) '*)
  )
)
(define (multiplier p) (car p))
(define (multiplicand p)
  (let ((right (cddr p)))
    (if (< (length right) 2)
      (car right)
      right
    )
  )
)

; seems hard to simplify ...
; (display (deriv '(x + (x + x) * 2 * y) 'x))(newline)
; (display (deriv '(x + (x + y + x) * 2 * y) 'x))(newline)
; (display (deriv '((x + x * y) + x * 2 + (x + x + x)) 'x))(newline)
