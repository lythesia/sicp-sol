(define (foo n)
  (cond ((< n 3) n)
        (else (+ (foo (- n 1)) (* 2 (foo (- n 2))) (* 3 (foo (- n 3)))))
  )
)

(define (foo-iter n_1 n_2 n_3 count)
  (if (= count 0)
    n_3
    (foo-iter (+ n_1 (* 2 n_2) (* 3 n_3)) n_1 n_2 (- count 1))
  )
)

(display (foo 4))(newline)
(display (foo-iter 2 1 0 4))(newline)
