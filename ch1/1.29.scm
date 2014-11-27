(define (sum term a next b)
  (if (> a b) 0
    (+ (term a) (sum term (next a) next b))
  )
)

(define (simpson-integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (coe k)
      (cond ((= k 0) 1)
            ((= k n) 1)
            ((even? k) 2)
            (else 4)
      )
    )

    (define (term k)
      (* (coe k) (f (+ a (* k h))))
    )

    (define (inc k)
      (+ k 1)
    )

    (* (/ h 3) (sum term 0 inc n))
  )
)

(define (cube n) (* n n n))

(display (simpson-integral cube 0 1 100))(newline)
(display (simpson-integral cube 0 1 1000))(newline)
