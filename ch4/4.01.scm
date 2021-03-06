(define (list-of-values-force-left exps env)
  (if (no-operands? exps)
    '()
    (let ((first (eval (first-operand exps) env))) ; force eval first arg
      (cons first (list-of-values-force-left (rest-operands exps) env))
    )
  )
)

(define (list-of-values-force-right exps env)
  (if (no-operands? exps)
    '()
    (let ((rest (list-of-values-force-right (rest-operands exps) env))) ; force eval rest arg (recursive-via-`list-of-values-force-right` to last and eval backing to first)
      (cons (eval (first-operand exps) env) rest)
    )
  )
)
