(define profile-stack 1)

(include "5.04.ec.scm")

; total-pushes: 3, maximum-depth: 3

; ;;; EC-Eval value:
; ok

; ;;; EC-Eval input:
; (factorial 3)

; total-pushes: 134, maximum-depth: 10

; ;;; EC-Eval value:
; 6

; ;;; EC-Eval input:
; (factorial 5)

; total-pushes: 204, maximum-depth: 10

; ;;; EC-Eval value:
; 120

; ;;; EC-Eval input:
; (factorial 10)

; total-pushes: 379, maximum-depth: 10

; ;;; EC-Eval value:
; 3628800

;; a) maximum-depth is always 10

;; b) total-pushes = 35 * n + 29
