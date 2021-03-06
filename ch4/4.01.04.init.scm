(define (setup-environment)
  ; need primitive-procedure, true and false
  (let ((initial-env (extend-environment (primitive-procedure-names) (primitive-procedure-objects) the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env
  )
)
(define the-global-environment (setup-environment))

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
    (list 'exit (lambda () (set! exit-flag 1)))
    ; .. other primitives
  )
)

(define (primitive-procedure-names)
  (map car primitive-procedures)
)
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures)
)

; make ref for apply?
; (define apply-in-underlying-scheme apply)
; then 4.01.01:apply will shadow it
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args) ; check 4.01.01
  ; so this is native apply (not that we implemented)
)

(define apply-in-underlying-scheme apply)
