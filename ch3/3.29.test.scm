(load "3.03.04.simula.scm")

(load "3.29.scm")

(define input-1 (make-wire))
(define input-2 (make-wire))
(define output (make-wire))

(or-gate input-1 input-2 output)
; (print-agenda the-agenda)

(display "\npropagating ...\n")
(propagate)
(display "final result: ")(display (get-signal output))(newline)

(set-signal! input-1 1) ; add action to agenda, because its only action, so output is correct
(display "\npropagating ...\n")
(propagate)
(display "final result: ")(display (get-signal output))(newline)
