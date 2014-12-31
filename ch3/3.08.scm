(define (f x)
  (set! f (lambda (arg) 0))
  x
)

; test
; (display (f 1))(newline) ; first time
; (display (f 2))(newline) ; after
; (display (f 3))(newline) ; after 2

; in scheme, apply from left to right
; only leave one line below to see diff
; (display (+ (f 0) (f 1)))(newline)
; (display (+ (f 1) (f 0)))(newline)
