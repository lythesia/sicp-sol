(load "4.06.scm")

; (let* ((<var-1> <exp-1>) (<var-2> <exp-2>) ..)
;   ..
; )
; |
; v
; (let ((<var-1> <exp-1>))
;   (let ((<var-2> <exp-2>))
;     ..
;   )
; )
(define (let*? exp) (tagged-list? exp 'let*))
(define first-binding car)
(define rest-bindings cdr)
(define bind-variable car)
(define bind-value cadr)
(define (let*->nested-lets exp)
  (conv-let* (let-bindings exp) (let-body exp))
)
(define (conv-let* bingdings body)
  (if (null? bindings) body
    (make-let (first-binding bindings) (conv-let* (rest-bindings bindings) body))
  )
)
(define (make-let bind body)
  (cons 'let (list bind) body)
)

; include in
(define (eval exp env)
  ; ..
  ((let*? exp) (eval (let*->nested-lets exp) env))
  ; ..
)
