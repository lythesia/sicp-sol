(load "3.27.memo.scm")

(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))
  )
)

(define memo-fib (memorize fib))

(display (memo-fib 30))(newline)
; real	0m12.015s
; user	0m11.783s
; sys	0m0.143s
