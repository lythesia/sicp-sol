(define extra-dispatches '(
  (test (op cond?) (reg exp))
  (branch (label ev-cond))
  (test (op let?) (reg exp))
  (branch (label ev-let))
))

(define extra-ev-labels '(
ev-cond
  (assign exp (op cond->if) (reg exp))
  (goto (label ev-if))
ev-let
  (assign exp (op let->combination) (reg exp))
  (perform (op user-print) (reg exp))
  (goto (label ev-application))
))

(include "5.04.ec.scm")
