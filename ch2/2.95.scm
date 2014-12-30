; a)
(define (pseudo-remainder-terms l1 l2)
  (let*(
        (t1 (first-term l1)) (t2 (first-term l2))
        (o1 (order t1)) (o2 (order t2))
        (c2 (coeff l2)) (scale (exp-g c2 (1+ (- o1 o2))))
       )
    (cdr (div-terms (mul-term-by-all-terms (make-term 0 scale) l1) l2))
  )
)
