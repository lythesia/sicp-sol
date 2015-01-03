(load "3.27.memo.scm")

(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (memo-fib (- n 1)) (memo-fib (- n 2))))
  )
)

(define memo-fib (memorize fib))

(display (memo-fib 30))(newline)
; real	0m0.023s
; user	0m0.023s
; sys	0m0.000s
