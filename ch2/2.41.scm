(load "2.2.3.scm")
(load "2.28.scm") ; for `reduce`

(define (unique-pairs n)
  (flatmap
    (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)
  )
)

; this is wrong, (enumerate-interval 1 0) will produce '()
(define (triple-combs-trivial n)
  (flatmap
    (lambda (i)
      (map 
        (lambda (j)
          (map (lambda (k) (list i j k)) (enumerate-interval 1 (1- j)))
        )
        (enumerate-interval 1 (1- i)) ; fix: (enumerate-interval 2 (1- i)), second largest should not < 2
      )
    )
    (enumerate-interval 1 n) ; fix: (enumerate-interval 3 n), largest should not < 3
  )
)

(define (triple-combs n)
  (flatmap
    (lambda (i) (map (lambda (j) (cons i j)) (unique-pairs (1- i))))
    (enumerate-interval 1 n)
  )
)

(define (unique-triples n)
  (flatmap permutations (triple-combs n))
)

(define (eql-sum? triple s)
  (= s (reduce + 0 triple))
)

(define (eql-sum-triples n s)
  (filter (lambda (t) (eql-sum? t s)) (unique-triples n))
)

; test
; (display (triple-combs-trivial 4))(newline)
; (display (triple-combs 4))(newline)
; (display (unique-triples 4))(newline)
; (display (eql-sum-triples 4 6))(newline)
