;; a)
ev-application
  (save continue)
  (assign unev (op operands) (reg exp))
  (assign exp (op operator) (reg exp))
  (test (op symbol?) (reg exp))
  (branch (label ev-appl-operator-symbol))
  (save env)
  (save unev)
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))
ev-appl-did-operator
  (restore unev)
  (restore env)
ev-appl-did-operator-symbol
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)
ev-appl-operator-symbol
  (assign continue (label ev-appl-did-operator-symbol))
  (goto (label eval-dispatch))

;; b)
;; 1. interpret will execute all case-select logic each time runs, while compiled code seq is exact and accurate, so slower vs faster.
;; 2. case logic will make code complex.
