(load "2.2.3.scm")
(load "../ch1/1.22.scm")

(define (unique-pairs n)
  (flatmap
    (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)
  )
)

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair)
  (let ((l (car pair)) (r (cadr pair)))
    (list l r (+ l r))
  )
)

(define (prime-sum-pairs n)
  (map
    make-pair-sum
    (filter prime-sum? (unique-pairs n))
  )
)

; test
; (display (prime-sum-pairs 6))(newline)
