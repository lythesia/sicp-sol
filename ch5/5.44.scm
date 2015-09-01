(define (non-overwrite? op env)
  (eq? (find-vairable op env) 'not-found)
)

; from 5.38.scm
(define old-open-code? open-code?)
(define (open-code? exp compile-time-env) (and (old-open-code? exp) (non-overwrite? exp compile-time-env)))

;; modified from 5.38.test.scm
(define (compile exp target linkage compile-time-env)
  (cond
    ; ..
    ((open-code? exp compile-time-env) (compile-open-code-arbi (operator exp) (operands exp) target linkage compile-time-env))
    ; overwrite goes here
    ((application? exp) (compile-application exp target linkage compile-time-env))
    (else (error "Unknown expression type -- COMPILE" exp))
  )
)
