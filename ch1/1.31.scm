(define (product-rec term a next b)
  (if (> a b) 1
    (* (term a) (product-rec term (next a) next b))
  )
)

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b) result
      (iter (next a) (* (term a) result)))
  )
  (iter a 1)
)

(define (factorial n)
  (define (identity n) n)
  (define (inc n) (+ n 1))
  (product-rec identity 1 inc n)
)

(define (pi n)
  (define (f x)
    (* (/ (- x 1) x) (/ (+ x 1) x))
  )

  (define (next k) (+ k 2.0))

  (* 4 (product-iter f (next 1) next n))
)

(display (factorial 6))(newline)
(display (pi 10000))(newline)
