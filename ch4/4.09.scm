(load "4.01.01.ea.scm")
(load "4.01.02.parse.scm")

; (while <cond> <body>)
; |
; v
; like name-let:
;   1. define loop
;   2. invoke loop
(define (while? exp) (tagged-list? exp 'while))
(define while-predicate cadr)
(define while-body cddr)
(define (while->combination exp)
  (sequence->exp
    (list
      ; 1. define
      (list 'define (list 'loop)
        (make-if
          (while-predicate exp)
          (sequence->exp (append (while-body exp) '(loop)))
          'end-while
        )
      )
      ; 2. invoke
      '(loop)
    )
  )
)

; include in
(define (eval exp env)
  ; ..
  ((while? exp) (eval (while->combination exp) env))
  ; ..
)
