(define dont-run 1)
(include "4.01.no-aly.scm")

;; override eval
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
    ((quoted? exp) (text-of-quotation exp))

    ; assignment/define
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))

    ; if
    ((if? exp) (eval-if exp env))

    ; lambda
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

;; override apply
(define (apply procedure arguments env) ; <- env is needed
  (cond
    ; simple
    ((primitive-procedure? procedure)
     (apply-primitive-procedure
       procedure
       (list-of-arg-values arguments env)
     )
    )

    ; compound
    ((compound-procedure? procedure)
      (eval-sequence
        (procedure-body procedure)
        (extend-environment
          (procedure-parameters procedure)
          (list-of-delayed-args arguments env)
          (procedure-environment procedure)
        )
      )
    )

    ; unhandled
    (else (error "Unknown procedure type -- APPLY" procedure))
  )
)

;; subs
(define (actual-value exp env)
  (force-it (eval exp env))
)

; eval-if is needed to be strict
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)
  )
)

(define (list-of-arg-values args env)
  (map (lambda (arg) (actual-value arg env)) args)
)
(define (list-of-delayed-args args env)
  (map (lambda (arg) (delay-it arg env)) args)
)

; "lazy" struct
(define (delay-it exp env)
  (list 'thunk exp env)
)
(define (thunk? obj)
  (tagged-list? obj 'thunk)
)
(define thunk-exp cadr)
(define thunk-env caddr)

; memorize
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk)
)
(define thunk-value cadr) ; should be called on evaluated-thunk
(define (force-it obj)
  (cond
    ((thunk? obj)
     (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
       ; 'evaluated-thunk <real-value> '()
       (set-car! obj 'evaluated-thunk)
       (set-car! (cdr obj) result)
       (set-cdr! (cdr obj) '()) ; <- set binded env to '() for sake of gc
       result
     )
    )
    ((evaluated-thunk? obj) (thunk-value obj))
    (else obj)
  )
)

;; driver
(define input-prompt ";;; L-EVAL input:")
(define output-prompt ";;; L-EVAL output:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read))) ; read wait for input
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)
    )
  )
  (if (= exit-flag 0)
    (driver-loop)
  )
)

(driver-loop)
