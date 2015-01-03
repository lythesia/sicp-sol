(load "3.27.memo.scm")

(define memo-fib
  (memorize
    (lambda (n)
      (cond
        ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (memo-fib (- n 1)) (memo-fib (- n 2))))
      )
    )
  )
)

(display (memo-fib 30))(newline)
; real	0m0.022s
; user	0m0.023s
; sys	0m0.000s
