; comb: combiner operator
; pred?: predicate for filter
; null-val: initial val for accmulate

(define (filtered-accmulate-rec comb pred? null-val term a next b)
  (if (> a b) null-val
    (let ((rest (filtered-accmulate-rec comb pred? null-val term (next a) next b)))
      (if (pred? a) (comb (term a) rest)
        rest)
    )
  )
)

(define (filtered-accmulate-iter comb pred? null-val term a next b)
  (define (iter a result)
    (if (> a b) result
      (if (pred? a) (iter (next a) (comb (term a) result))
        (iter (next a) result)
      )
    )
  )
  (iter a null-val)
)

(define (sum-int-odd a b)
  (filtered-accmulate-rec + odd? 0 (lambda (x) x) a (lambda (x) (+ x 1)) b)
)

(define (coprime? k n)
  (and (< k n) (= (gcd k n) 1))
)

(define (product-coprime-n n)
  (filtered-accmulate-iter * (lambda (x) (coprime? x n)) 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n)
)

(display (sum-int-odd 1 10))(newline)
(display (product-coprime-n 10))(newline)
