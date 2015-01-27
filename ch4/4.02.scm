; a) (define x 3) will interpret as call `define` procedure with `x` and `3`

; b) do following modify:
; (define (application? exp) (tagged-list? exp 'call))
; (define (operator exp) (cadr exp))
; (define (operands exp) (cddr exp))
