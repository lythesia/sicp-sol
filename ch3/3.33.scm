(load "util.scm")
(load "3.03.05.connector.scm")
(load "3.03.05.constraint.scm")
(load "3.03.05.probe.scm")

; average constraint
(define (average a b c)
  (let ((ab (make-connector)) (d (make-connector)))
    (adder a b ab)
    (constant 2 d)
    (multiplier c d ab)
    'ok
  )
)

; test
; (define A (make-connector))
; (define B (make-connector))
; (define C (make-connector))
; (average A B C)

; (probe "A" A)
; (probe "B" B)
; (probe "C" C)

; (set-value! A 10 'user)
; (set-value! B 20 'user)
