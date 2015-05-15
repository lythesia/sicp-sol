; ('let ([<var-1> <exp-1>])
;   <body>
; )
; |
; v
; (('lambda ([var]) <body>) [exp])
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
  (let ((bindings (let-bindings exp)))
    (cons
      (make-lambda (let-variables bindings) (let-body exp))
      (let-values bindings)
    )
  )
)

;; install
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
