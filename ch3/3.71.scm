(load "3.70.scm")

(define (weight-cube-sum p)
  (let ((i (car p)) (j (cadr p)))
    (+ (* i i i) (* j j j))
  )
)

(define (ramanujan pairs)
  (define (iter prev rest)
    (if (empty-stream? rest) the-empty-stream
      (let* ((x (stream-car rest)) (s (weight-cube-sum x)))
        (if (= prev s)
          (cons-stream s (iter 0 (stream-cdr rest)))
          (iter s (stream-cdr rest))
        )
      )
    )
  )
  (iter 0 pairs)
)

; test
; (define r (ramanujan (weighted-pairs integers integers weight-cube-sum)))
; (stream-head r 6)(newline)
