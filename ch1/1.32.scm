; comb: combiner operator
; null-val: initial val for accmulate

(define (accmulate-rec comb null-val term a next b)
  (if (> a b) null-val
    (comb (term a) (accmulate-rec comb null-val term (next a) next b))
  )
)

(define (accmulate-iter comb null-val term a next b)
  (define (iter a result)
    (if (> a b) result
      (iter (next a) (comb (term a) result))
    )
  )
  (iter a null-val)
)

(define (identity x) x)
(define (inc x) (+ x 1))

(define (sum-int a b)
  (accmulate-rec + 0 identity a inc b)
)

(define (product-int a b)
  (accmulate-iter * 1 identity a inc b)
)

(display (sum-int 1 10))(newline)
(display (product-int 1 6))(newline)
