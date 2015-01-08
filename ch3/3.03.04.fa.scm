(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ha
  )
)

(define (full-adder a b cin s cout)
  (let ((s1 (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b cin s1 c1)
    (half-adder a s1 s c2)
    (or-gate c1 c2 cout)
    'fa
  )
)
