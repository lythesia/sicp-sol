(define extra-dispatches '(
  (test (op cond?) (reg exp))
  (branch (label ev-cond))
))

(define extra-ev-labels '(
ev-cond
  (save continue)
  (assign unev (op cond-clauses) (reg exp))
ev-cond-loop
  (test (op cond-no-clauses?) (reg unev))
  (branch (label ev-cond-no-match))
  (assign exp (op cond-first-clause-predicate) (reg unev))
  (save unev)
  (save env)
  (assign continue (label ev-cond-decide))
  (goto (label eval-dispatch))
ev-cond-decide
  (restore env)
  (restore unev)
  (test (op true?) (reg val))
  (branch (label ev-cond-consequent))
ev-cond-alternative
  (assign unev (op cond-rest-clauses) (reg unev))
  (goto (label ev-cond-loop))
ev-cond-consequent
  (assign unev (op cond-first-clause-actions) (reg unev))
  (goto (label ev-sequence))
ev-cond-no-match
  (assign val (const #f))
  (restore continue)
  (goto (reg continue))
))

(include "5.04.ec.scm")
