(load "3.03.04.wire.scm")
(load "3.03.04.agenda.scm")
(load "3.03.04.drive.scm")

(load "3.03.04.inv.scm")
(load "3.03.04.and.scm")
(load "3.28.scm") ; or-gate

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
