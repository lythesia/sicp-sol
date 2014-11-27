(define (fast-expt b n)
  (expt-iter b n 1)
)

(define (expt-iter b n a)
  (cond ((= n 0 ) a)
        ((even? n) (expt-iter (* b b) (/ n 2) a))
        (else (expt-iter b (- n 1) (* a b)))
  )
)

(define (even? n)
  (= (remainder n 2) 0)
)

; (,trace expt-iter)
; (display (fast-expt 2 8))(newline)
; (display (fast-expt 2 9))(newline)
