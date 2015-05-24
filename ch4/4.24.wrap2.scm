(use-modules (ice-9 time))
(define dont-run-all 1)
(include "4.01.aly.scm")

(time (driver-loop))
