(use-modules (ice-9 time))

(define dont-run 1)
(include "4.01.no-aly.scm")

(time (driver-loop))
