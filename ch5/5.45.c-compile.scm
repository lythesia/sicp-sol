;; with no compile-time lexical address support

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
    ; with open-code
    ((open-code? exp) (compile-open-code-arbi (operator exp) (operands exp) target linkage))
    ((application? exp) (compile-application exp target linkage))
    (else (error "Unknown expression type -- COMPILE" exp))
  )
)

(define profile-stack 1)
(define open-code 1)
(load "5.05.load-eceval-compiler.scm")

(compile-and-go
  '(define (factorial n)
      (if (= n 1) 1 (* (factorial (- n 1)) n))
   )
)
; after execute external-entry's codes, we comeback repl entry, then we can call factorial

; in eceval repl:
; total-pushes: 13, maximum-depth: 8
; total-pushes: 23, maximum-depth: 18
; total-pushes: 53, maximum-depth: 48
; total-pushes: 103, maximum-depth: 98
; total-pushes: 203, maximum-depth: 198
; total-pushes: 503, maximum-depth: 498
; total-pushes: 1003, maximum-depth: 998
; total-pushes: 2003, maximum-depth: 1998
