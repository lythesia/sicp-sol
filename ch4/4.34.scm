(define dont-run-any 1)
(include "4.33.scm")

#|
; preserve primitive scheme cons/car/cdr
(map
  (lambda (name proc) (define-variable! name (list 'primitive proc) the-global-environment))
  (list 'scm-cons 'scm-car 'scm-cdr)
  (list cons car cdr)
)

; install tagged ones into env
(actual-value
  '(begin
     (define (cons x y)
       (scm-cons 'cons (lambda (m) (m x y)))
     )
     (define (car z)
       ((scm-cdr z) (lambda (p q) p))
     )
     (define (cdr z)
       ((scm-cdr z) (lambda (p q) q))
     )
  )
  the-global-environment
)

(define (disp-cons obj depth)
  (letrec
    ((user-car (lambda (z) (force-it (lookup-variable-value 'x (procedure-environment (cdr z))))))
     (user-cdr (lambda (z) (force-it (lookup-variable-value 'y (procedure-environment (cdr z)))))))
    (cond
      ((>= depth 10) (display "...)"))
      ((null? obj) (display ""))
      (else
        (let ((cdr-val (user-cdr obj)))
          (display "(")
          (display (user-car obj))
          (if (tagged-list? cdr-val 'cons)
            (begin
              (display " ")
              (disp-cons cdr-val (+ depth 1))
            )
            (begin
              (display " . ")
              (display cdr-val)
            )
          )
          (display ")")
        )
      )
    )
  )
)

(define (user-print obj)
  (if (compound-procedure? obj)
    (display
      (list
        'compound-procedure
        (procedure-parameters obj)
        (procedure-body obj)
        '<procedure-env>
      )
    )
    (if (tagged-list? obj 'cons)
      (disp-cons obj 0)
      (display obj)
    )
  )
)
(driver-loop)
|#

(define (eval exp env)
  (cond
    ; base {{
    ; self-eval
    ((self-evaluating? exp) exp)

    ; lookup var
    ((variable? exp) (lookup-variable-value exp env))
    ; }}

    ; special {{
    ; quotation
    ((quoted? exp) (text-of-quotation exp env))

    ; assignment/define
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))

    ; if
    ((if? exp) (eval-if exp env))

    ; lambda
    ((list-lambda? exp) (make-list-procedure (lambda-parameters exp) (lambda-body exp) env))
    ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))

    ; begin
    ((begin? exp) (eval-sequence (begin-actions exp) env))

    ; cond
    ((cond? exp) (eval (cond->if exp) env))

    ; let
    ((let? exp) (eval (let->combination exp) env))

    ; apply
    ((application? exp) (apply (actual-value (operator exp) env) (operands exp) env)) ; <- operator is `actual-value` handeld? yes, since itself can a procedure returned from a compound procedure
    ; }}

    ; unhandled
    (else (error "Unknown expression type -- EVAL" exp))
  )
)

;; list-lambda: ('list-lambda (..) ..) just like lambda
;; list-procedure: ('list-procedure <paras> <body> <env>) as for cons..
;;  ('list-procedure m <..> the-global-environment + cons' x&y)
(define (list-lambda? exp) (tagged-list? exp 'list-lambda))
(define (make-list-procedure parameters body env)
  (list 'list-procedure parameters body env)
)
;; override
(define (compound-procedure? p)
  (or (normal-procedure? p) (list-procedure? p))
)
(define (normal-procedure? p) (tagged-list? p 'procedure))
(define (list-procedure? p) (tagged-list? p 'list-procedure))

(define LIST-MAX-DEPTH 5)
(define (user-print obj)
  (cond
    ((normal-procedure? obj)
     (display (list 'compound-procedure (procedure-parameters obj) (procedure-body obj) '<procedure-env>)))
    ((list-procedure? obj)
     (display (list-proc->list obj LIST-MAX-DEPTH)))
    (else (display obj))
  )
)
(define (list-proc->list list-proc count)
  (define (apply-proc-to-list proc lst env)
    (eval-sequence
      (procedure-body proc) ; cons/car/cdr body
      (extend-environment (procedure-parameters proc) lst (procedure-environment proc)) ; since cons return a list-procedure, which has only one parameter 'm, so (list list-proc) only feed one arg, that is list-procedure
    )
  )
  (define (list-element opt) ; opt as symbol -> get the true proc via actual-value (lookup in global env)
    (force-it
      (apply-proc-to-list (actual-value opt the-global-environment) (list list-proc) the-global-environment)
    )
  )
  (define (make-it-normal x n)
    (if (list-procedure? x)
      (if (zero? n)
        '(...)
        (list-proc->list x n) ; recursive
      )
      x
    )
  )
  (cons (make-it-normal (list-element 'car) LIST-MAX-DEPTH) ; here is MAX not count since car may be a list-procedure, too
        (make-it-normal (list-element 'cdr) (- count 1)))
)

; ; install tagged ones into env
(actual-value
  '(begin
     (define (cons x y)
       (list-lambda (m) (m x y))
     )
     (define (car z)
       (z (lambda (p q) p))
     )
     (define (cdr z)
       (z (lambda (p q) q))
     )
  )
  the-global-environment
)

(driver-loop)
