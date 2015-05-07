;; eval
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

    ; apply
    ((application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
    ; }}

    ; unhandled
    (else (error "Unknown expression type -- EVAL" exp))
  )
)

; sub-helpers
; argument
(define (list-of-values exps env)
  (if (no-operands? exps) '()
    ; this cons determines order of arg-eval!
    (cons
      (eval (first-operand exps) env)
      (list-of-values (rest-operands exps) env)
    )
  )
)

; condition
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)
  )
)

; sequence
(define (eval-sequence exps env)
  (cond
    ((last-exp? exps) (eval (first-exp exps) env))
    (else (eval (first-exp exps) env) (eval-sequence (rest-exps exps) env))
  )
)

; assignment/define
(define (eval-assignment exp env)
  (set-variable-value!
    (assignment-variable exp)
    (eval (assignment-value exp) env)
    env
  )
)
(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env
  )
  'ok ; alternative because no specification in standard
)

;; apply
(define (apply procedure arguments)
  (cond
    ; simple
    ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))

    ; compound
    ((compound-procedure? procedure)
      (eval-sequence
        (procedure-body procedure)
        (extend-environment                 ; do extend env
          (procedure-parameters procedure)  ; name param
          arguments                         ; real arg
          (procedure-environment procedure) ; parent env
        )
      )
    )

    ; unhandled
    (else (error "Unknown procedure type -- APPLY" procedure))
  )
)
