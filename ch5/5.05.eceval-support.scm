(load "5.05.syntax.scm")
;; makeup for syntax
; cond
(define cond-first-clause car)
(define cond-first-clause-predicate caar)
(define cond-first-clause-actions cdar)
(define cond-rest-clauses cdr)
(define cond-no-clauses? null?)
; apply
(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))

;; operation for machine use
;; from ch4/4.01.no-aly.scm
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

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env)
)
(define (compiled-procedure? proc) (tagged-list? proc 'compiled-procedure))
(define compiled-procedure-entry cadr)
(define compiled-procedure-env caddr)

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
    (list 'not not)
    (list 'eq? eq?)
    (list 'remainder remainder)
    (list 'display display)
    (list 'newline newline)
    (list 'read read)
    (list 'write write)
    (list 'list list)
    (list 'length length)
    (list 'member member)
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
; (define the-global-environment (setup-environment))
; (define (get-global-environment) the-global-environment)

; for repl
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
  (cond
    ((compound-procedure? object)
      (display
        (list
          'compound-procedure           ; check 4.01.03
          (procedure-parameters object)
          (procedure-body object)
          '<procedure-env>
        )
      )
    )
    ((compiled-procedure? object)
     (display object)(newline)  ; TODO
    )
    ((not (or (null? object) (unspecified? object))) ; ordinary object
      (display object)(newline)
    )
  )
)
