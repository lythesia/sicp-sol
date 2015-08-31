;; from right to left, in construct-arglist: (operand-codes (reverse operand-codes)), we first reverse original operands, and then one-by-one

;; we can do left to right as:
(define (construct-arglist operand-codes)
  (if (null? operand-codes)
    (make-instruction-sequence '() '(argl) '((assign argl (const ()))))
    (let ((code-to-get-last-arg
            (append-instruction-sequences
              (car operand-codes)
              (make-instruction-sequence '(val) '(argl) '((assign argl (op list) (reg val))))
            ))) ; compile first arg
      (if (null? (cdr operand-codes))
        code-to-get-last-arg
        (tack-on-instruction-sequence
          (preserving '(env) code-to-get-last-arg (code-to-get-rest-args (cdr operand-codes)))
          (make-instruction-sequence '() '() `((assign argl (op reverse) (reg argl))))  ; dont need, simply tack
        )
      )
    )
  )
)
