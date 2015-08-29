; ;;; EC-Eval input:
; (fib 1)

; total-pushes: 16, maximum-depth: 8

; ;;; EC-Eval value:
; 1

; ;;; EC-Eval input:
; (fib 2)

; total-pushes: 16, maximum-depth: 8

; ;;; EC-Eval value:
; 1

; ;;; EC-Eval input:
; (fib 3)

; total-pushes: 72, maximum-depth: 13

; ;;; EC-Eval value:
; 2

; ;;; EC-Eval input:
; (fib 4)

; total-pushes: 128, maximum-depth: 18

; ;;; EC-Eval value:
; 3

; ;;; EC-Eval input:
; (fib 5)

; total-pushes: 240, maximum-depth: 23

; ;;; EC-Eval value:
; 5

;; a)
;; maximum-depth = 5*n - 2 (n >= 2)

;; b)
;; S(n) = S(n-1) + S(n-2) + 40 (n > 2)
;; S(n) = 56 * f(n + 1) - 40
