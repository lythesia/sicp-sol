(define (pascal m n)
  (cond ((or (= n 0) (= m n)) 1)
        ((> n m) (error "invalid"))
        (else (+ (pascal (- m 1) (- n 1))
                 (pascal (- m 1) n)
              )
        )
  )
)

(define (fact n)
  (fact-iter 1 1 n)
)

(define (fact-iter p i c)
  (if (> i c)
    p
    (fact-iter (* p i) (+ i 1) c)
  )
)

(define (fast-pascal m n)
  (/ (fact m) (* (fact n) (fact (- m n))))
)

(display (pascal 4 2))(newline)
(display (fast-pascal 4 2))(newline)
