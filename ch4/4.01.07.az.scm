;; using analyze transform exp to a procedure that can take env to evaluate!

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
    
    ((application? exp) (analyze-application exp))

    (else (error "Unknown expression type -- ANALYZE" exp))
  )
)

;; predicates same with 4.01.02.parse.scm
;; inside <> is an object! in scheme pair/list

;; trivial
(define (self-evaluating? exp)
  (cond
    ((number? exp) #t)
    ((string? exp) #t)
    (else #f)
  )
)
(define (analyze-self-evaluating exp)
  (lambda (env) exp)
)

;; variable (dependent on env on each lookup)
(define (variable? exp)
  symbol? exp
)
(define (analyze-variable exp)
  (lambda (env)
    (lookup-variable-value exp env) ; `lookup-variable-value` from 4.01.03.env.scm
  )
)

;; quotation
(define (tagged-list? exp tag)
  (if (pair? exp)
    (= tag (car exp))
    #f
  )
)
; ('quote <exp>)
; 'a -> ('quote a); '(a b c) -> ('quote (a b c)); ..
(define (quoted? exp)
  (tagged-list? exp 'quote)
)
(define (text-of-quotation exp) (cadr exp))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)
  )
)

;; assignment
; ('set! <var> <val>)
(define (assignment? exp)
  (tagged-list? exp 'set!)
)
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp)) (vproc (analyze (assignment-value exp)))) ; value eval also need env
    (lambda (env)
      (set-variable-value! var (vproc env) env) ; `set-variable-value!` from 4.01.03.env.scm
      'ok
    )
  )
)

;; definition
; ('define <sym> <val>)
; ('define (<sym> [param]) [body])
(define (definition? exp)
  (tagged-list? exp 'define)
)
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)  ; var   ('define sym val)
    (caadr exp) ; proc  ('define (sym [param]) [body])
  )
)
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp) (cddr exp))
  )
)
(define (analyze-definition exp)
  (let ((var (definition-variable exp)) (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)  ; `define-variable!` from 4.01.03.env.scm
    )
  )
)

;; if
; ('if <pred> <cons> <alter>)
(define (if? exp)
  (tagged-list? exp 'if)
)
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false  ; our implemention
  )
)
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative)
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

;; lambda
; ('lambda <params> [body])
(define (lambda? exp)
  (tagged-list? exp 'lambda)
)
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))
(define (analyze-lambda exp)
  (let ((params (lambda-parameters exp)) (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env)
      (make-procedure params bproc env) ; `make-procedure` from 4.01.03.env.scm
    )
  )
)

;; begin
; ('begin [seq])
(define (begin? exp)
  (tagged-list? exp 'begin)
)
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond
    ((null? seq) seq)
    ((last-exp? seq) (first-exp seq))
    (else (make-begin seq))
  )
)
(define (make-begin seq) (cons 'begin seq))
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

;; cond
; ('cond [clause])
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp))
)
(define (expand-clauses clauses)
  (if (null? clauses)
    'false  ; no else (but what if whole as empty?)
    (let ((first (car clauses)) (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause is not last -- COND->IF" clauses)
        )
        (make-if
          (cond-predicate first)                ; pred
          (sequence->exp (cond-actions first))  ; cons
          (expand-clauses rest)                 ; alter
        )
      )
    )
  )
)

;; application
; (<sym> [arg])
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
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
     (procedure-body proc)
     (extend-environment
       (procedure-parameters proc)
       args
       (procedure-environment proc)
     )
    )

    (else (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))
  )
)
