(define dont-run 1)
(include "4.01.no-aly.scm") ; include is like C's #include, while load is like dlopen

; override no-aly's eval
(define (eval exp env)
  ((analyze exp) env)
)

(define (analyze exp)
  (cond
    ((self-evaluating? exp) (analyze-self-evaluating exp))

    ((variable? exp) (analyze-variable exp))

    ((quoted? exp) (analyze-quoted exp))

    ((assignment? exp) (analyze-assignment exp))
    ((definition? exp) (analyze-definition exp))

    ((if? exp) (analyze-if exp))

    ((lambda? exp) (analyze-lambda exp))

    ((begin? exp) (analyze-sequence (begin-actions exp)))
    
    ((cond? exp) (analyze (cond->if exp)))

    ((let? exp) (analyze (let->combination exp)))
    
    ((application? exp) (analyze-application exp))

    (else (error "Unknown expression type -- ANALYZE" exp))
  )
)

;; sub-helpers
(define (analyze-self-evaluating exp)
  (lambda (env) exp)
)

(define (analyze-variable exp)
  (lambda (env)
    (lookup-variable-value exp env) ; `lookup-variable-value` from 4.01.03.env.scm
  )
)

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)
  )
)

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp)) (vproc (analyze (assignment-value exp)))) ; value eval also need env
    (lambda (env)
      (set-variable-value! var (vproc env) env) ; `set-variable-value!` from 4.01.03.env.scm
      'ok
    )
  )
)

(define (analyze-definition exp)
  (let ((var (definition-variable exp)) (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)  ; `define-variable!` from 4.01.03.env.scm
      'ok
    )
  )
)

(define (analyze-if exp)
  (let
    ((p-proc (analyze (if-predicate exp)))
     (c-proc (analyze (if-consequent exp)))
     (a-proc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (p-proc env))  ; `true?` from 4.01.03.env.scm
        (c-proc env)
        (a-proc env)
      )
    )
  )
)

(define (analyze-lambda exp)
  (let ((params (lambda-parameters exp)) (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env)
      (make-procedure params bproc env) ; `make-procedure` from 4.01.03.env.scm
    )
  )
)

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env))
  )

  (define (loop first-proc rest-procs) ; from head, (((1 + 1) + 1) + 1 ..)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs)) (cdr rest-procs))
    )
  )

  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE")
    )
    (loop (car procs) (cdr procs))
  )
)

(define (analyze-application exp)
  (let ((f-proc (analyze (operator exp)))
        (a-procs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
        (f-proc env)
        (map (lambda (a-proc) (a-proc env)) a-procs)
      )
    )
  )
)
(define (execute-application proc args)
  (cond
    ; trivial
    ((primitive-procedure? proc) (apply-primitive-procedure proc args)) ; from 4.01.04.init.scm

    ; compound
    ; `procedure-body`, `procedure-parameters`, `procedure-environment`,
    ; `extend-environment` from 4.01.03.env.scm
    ((compound-procedure? proc)
     ((procedure-body proc) ; proc
      (extend-environment ; env
        (procedure-parameters proc)
        args
        (procedure-environment proc)
      )
     )
    )

    (else (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))
  )
)

(if (not (defined? 'dont-run-all))
  (driver-loop)
)
