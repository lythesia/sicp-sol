(load "util.scm")
(load "3.03.05.connector.scm")
(load "3.03.05.constraint.scm")
(load "3.03.05.probe.scm")

(define (cel-fah-conv c f)
  (let
    ((u (make-connector))
     (v (make-connector))
     (w (make-connector))
     (x (make-connector))
     (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok
  )
)

(define C (make-connector))
(define F (make-connector))
(cel-fah-conv C F)

(probe "cel-temp" C)
(probe "fah-temp" F)

(set-value! C 25 'user)

(forget-value! C 'user)

; C(25) --> #f ( cel = ? )
; each -> multiplier_1 
;         |-- u(225) --> #f
;         |   each -> multiplier_2
;         |           |-- u(225) --> ignore 225
;         |           |-- v(45) -- #f
;         |           |   each -> adder_1
;         |           |           |-- F(77) --> #f ( fah = ? )
;         |           |           |-- v(45) --> ignore 45
;         |           |           `-- y(32) --> ignore 32
;         |           `-- x(5) --> ignore 5
;         |-- C(25) --> ignore 25
;         `-- w(9) --> ignore 9
