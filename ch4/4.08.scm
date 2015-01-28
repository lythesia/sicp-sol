(load "4.06.scm")

(define (name-let? exp) (symbol? (cadr exp))) ; var
(define name-let-variable cadr)
(define name-let-bindings caddr)
(define name-let-body cdddr)
(define (let->combination exp)
  (if (name-let? exp)
    ; name let:
    ;   1. define lambda
    ;   2. invoke it
    (let ((name (name-let-variable exp)) (bindings (name-let-bindings exp)) (body (name-let-body exp)))
      (sequence->exp
        (list
          ; 1. define
          (list 'define name (make-lambda (let-variables bindings) body))
          ; 2. invoke
          (cons name (let-values bindings))
        )
      )
    )
    ; simple let
    (conv-let (let-bindings exp) (let-body exp))
  )
)

; include in before `application?` since (let <var> <bindings> <body>) has same form with application
(define (eval exp env)
  ; ..
  ((let? exp) (eval (let->combination exp) env))
  ; ..
)
