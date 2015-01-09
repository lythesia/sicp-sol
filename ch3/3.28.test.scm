(load "3.03.04.simula.scm")

(define input-1 (make-wire))
(define input-2 (make-wire))
(define output (make-wire))

(or-gate input-1 input-2 output)
(print-agenda the-agenda)

(propagate)
(display (get-signal output))(newline)

(set-signal! input-1 1)
(print-agenda the-agenda)
(propagate)
(display (get-signal output))(newline)
