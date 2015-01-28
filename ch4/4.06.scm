(load "4.01.01.ea.scm")
(load "4.01.02.parse.scm")

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
  (if (null? bindings) '()
    (cons (caar bindings) (let-variables (cdr bindings)))
  )
  ; simpler
  ; (map car bindings)
)
(define (let-values bindings)
  (if (null? bindings) '()
    (cons (cadar bindings) (let-values (cdr bindings)))
  )
  ; simpler
  ; (map cadr bindings)
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

; include in
(define (eval exp env)
  ; ..
  ((let? exp) (eval (let->combination exp) env))
  ; ..
)
