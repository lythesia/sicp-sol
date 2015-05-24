;; parse
; self-eval
(define (self-evaluating? exp)
  (cond
    ((number? exp) #t)
    ((string? exp) #t)
    (else #f)
  )
)

; variable
(define (variable? exp) (symbol? exp))

; helper
(define (tagged-list? exp sym)
  (if (pair? exp) (eq? (car exp) sym) #f)
)

; quotation
; : quote text
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

; assignment
; : set! var val
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; definition
; : define var val
; : define (var param ..) body
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)  ; var
    (caadr exp) ; proc
  )
)
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp) (cddr exp))
  )
)

; lambda
; : (lambda (param body))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

; if
; : if pred cons alter
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false
  )
)
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative)
)

; begin
; : begin seq ..
(define (begin? exp) (tagged-list? exp 'begin))
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

; apply
; : (foo)
; : (foo bar ..)
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; cond (derived from if)
; : cond clauses
;         : pred action ..
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

; (let ((<var-1> <exp-1>) (<var-2> <exp-2>) ..)
;   ..
; )
; |
; v
; ((lambda (<var-1> <var-2> ..) ..) <exp-1> <exp-2> ..)
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-variables bindings)
  (map car bindings)
)
(define (let-values bindings)
  (map cadr bindings)
)
(define (let->combination exp)
  (conv-let (let-bindings exp) (let-body exp))
)
(define (conv-let bindings body)
  ; make application
  (cons
    (make-lambda (let-variables bindings) body)
    (let-values bindings)
  )
)

;; ============
;; env
(define (true? x)
  (not (eq? x #f))
)

(define (false? x)
  (eq? x #f)
)

(define (make-procedure parameters body env)
  (list 'procedure parameters body env)
)
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define procedure-parameters cadr)
(define procedure-body caddr)
(define procedure-environment cadddr)

; def env
;   (frame . outer-env)
(define (enclosing-environment env) (cdr env))  ; outer
(define (first-frame env) (car env))
(define the-empty-environment '())

; list-struct:
;   (var-1 var-2 ..) . (val-1 val-2 ..)
(define (make-frame variables values) (cons variables values))
(define frame-variables car)
(define frame-values cdr)
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame)))
)

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals)
    )
  )
)

; O(n)
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond
        ; current env not found, goto outer
        ((null? vars) (env-loop (enclosing-environment env)))
        ; found
        ((eq? var (car vars)) (car vals))
        ; iter
        (else (scan (cdr vars) (cdr vals)))
      )
    )
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame) (frame-values frame))
      )
    )
  )
  (env-loop env)
)
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond
        ((null? vars) (env-loop (enclosing-environment env)))
        ((eq? var (car vars)) (set-car! vals val))
        (else (scan (cdr vars) (cdr vals)))
      )
    )
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame) (frame-values frame))
      )
    )
  )
  (env-loop env)
)
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond
        ((null? vars) (add-binding-to-frame! var val frame))
        ((eq? var (car vars)) (set-car! vals val))
        (else (scan (cdr vars) (cdr vals)))
      )
    )
    (scan (frame-variables frame) (frame-values frame))
  )
)

;; ============
;; init
; primitive-procedure:
;   'primitive (impl)
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive)
)
(define (primitive-implementation proc)
  (cadr proc)
)

; table:
;   <name> <proc>(these procs are primitive, not written in scm)
(define primitive-procedures
  (list
    ; 
    (list 'car car)
    (list 'cdr cdr)
    (list 'cons cons)
    (list 'null? null?)
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)
    (list '= =)
    (list '> >)
    (list '< <)
    (list '<= <=)
    (list '>= >=)
    (list 'eq? eq?)
    (list 'remainder remainder)
    (list 'display display)
    (list 'newline newline)
    (list 'read read)
    (list 'write write)
    (list 'list list)
    (list 'length length)
    (list 'exit (lambda () (set! exit-flag 1) 'bye~))
    ; .. other primitives
  )
)

(define (primitive-procedure-names)
  (map car primitive-procedures)
)
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures)
)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args)
)

; using scheme self's `apply`
(define apply-in-underlying-scheme apply)

(define (setup-environment)
  ; need primitive-procedure, true and false
  (let ((initial-env (extend-environment (primitive-procedure-names) (primitive-procedure-objects) the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env
  )
)

;; ============
;; eval/apply
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

    ; let
    ((let? exp) (eval (let->combination exp) env))

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

;; ============
;; dirver
(define input-prompt ";;; M-EVAL input:")
(define output-prompt ";;; M-EVAL output:")

(define exit-flag 0)

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read))) ; read wait for input
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)
    )
  )
  (if (= exit-flag 0)
    (driver-loop)
  )
)

(define (prompt-for-input string)
  (newline)(newline)
  (display string)
  (newline)
)
(define (announce-output string)
  (newline)
  (display string)
  (newline)
)
(define (user-print object)
  (if (compound-procedure? object)
    (display
      (list
        'compound-procedure           ; check 4.01.03
        (procedure-parameters object)
        (procedure-body object)
        '<procedure-env>
      )
    )
    (if (not (or (null? object) (unspecified? object)))
      (display object)(newline)
    )
  )
)
(define the-global-environment (setup-environment))

(if (not (defined? 'dont-run))
  (driver-loop)
)
