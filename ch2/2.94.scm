; inside polynomial package
(define (remainder-terms l1 l2)
  (cdr (div-terms l1 l2))
)
(define (gcd-terms l1 l2)
  (if (empty-list? l2) l1
    (gcd-terms l2 (remainder-terms l1 l2))
  )
)

(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
    (error "Polys not in same var -- GCD-POLY" (list p1 p2))
  )
)

(put 'gcd 'polynomial (lambda (x y) (tag (gcd-poly x y))))

(define (greatest-common-divisor x y) (apply-generic 'gcd x y))
