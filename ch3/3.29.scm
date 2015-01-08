(define (or-gate x y output)
  (let ((invx (make-wire)) (invy (make-wire)) (invxy (make-wire)))
    (inverter x invx)
    (inverter y invy)
    (and-gate invx invy invxy)
    (inverter invxy output)
    'or
  )
)
