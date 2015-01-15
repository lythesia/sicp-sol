(load "3.05.01.stream.scm")

; expand digits of `n/den` (result as float) under base `radix`
(define (expand n den radix)
  (cons-stream (quotient (* n radix) den) (expand (remainder (* n radix) den) den radix))
)

; test
; (display-stream-to (expand 1 7 10) 10)(newline)
; (display-stream-to (expand 3 8 10) 10)(newline)
