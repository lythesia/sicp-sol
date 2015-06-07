(define dont-run-now 1)
(include "4.02.03.full.scm")

(define old-eval eval)

(define (eval exp env)
  (if (quoted? exp)
    (text-of-quotation exp env)
    (old-eval exp env)
  )
)

(define (text-of-quotation exp env)
  (let ((text (cadr exp)))
    (if (pair? text)
      (eval (make-list text) env)
      text
    )
  )
)

(define (make-list exp)
  (if (null? exp)
    (list 'quote '())
    ; make `cons` struct for eval
    ; '(cons (quote <1st>) (cons (quote <2nd> ..)))
    (list 'cons (list 'quote (car exp)) (make-list (cdr exp)))
  )
)

(if (not (defined? 'dont-run-any))
  (driver-loop)
)
; >> (list-ref integers 17) ; 18
; >> (car '(a b c)) ; a
