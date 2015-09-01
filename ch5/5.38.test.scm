(include "5.05.compile.scm")
(include "5.38.scm")

(define (compile exp target linkage)
  (cond
    ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
    ((quoted? exp) (compile-quoted exp target linkage))
    ((variable? exp) (compile-variable exp target linkage))
    ((assignment? exp) (compile-assignment exp target linkage))
    ((definition? exp) (compile-definition exp target linkage))
    ((if? exp) (compile-if exp target linkage))
    ((lambda? exp) (compile-lambda exp target linkage))
    ((begin? exp) (compile-sequence (begin-actions exp) target linkage))
    ((cond? exp) (compile (cond->if exp) target linkage))
    ((open-code? exp) (compile-open-code-arbi (operator exp) (operands exp) target linkage))
    ((application? exp) (compile-application exp target linkage))
    (else (error "Unknown expression type -- COMPILE" exp))
  )
)

(define compiled-instruction-sequence
  (compile
    '(define (factorial n)
       (if (= n 1)
         1
         (* (factorial (- n 1)) n)))
    'val
    'next
  )
)

(map (lambda (x) (display x)(newline)) (statements compiled-instruction-sequence))


;; test arbi
(display "(+ _ _ _)\n")
(map (lambda (x) (display x)(newline)) (statements (compile '(+ 1 (+ 2 3) x) 'val 'next)))
(display "(* _ _ _)\n")
(map (lambda (x) (display x)(newline)) (statements (compile '(* 1 (g x) x) 'val 'next)))
