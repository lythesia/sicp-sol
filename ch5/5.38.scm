;; a)
; should act like construct-arglist
(define (spread-arguments operands)
  (if (= (length operands) 2)
    (let ((op1 (compile (car operands) 'arg1 'next)) (op2 (compile (cadr operands) 'arg2 'next)))
      (preserving
        '(env)
        op1
        (preserving '(arg1) op2 (empty-instruction-sequence))
      )
    )
    (error "Spread arguments operation require exact 2 operands -- COMPILE" operands)
  )
)

;; b)
(define (open-code? exp) (memq (operator exp) '(+ - * =)))

(define (compile-open-code operator operands target linkage)
  (end-with-linkage
    linkage
    (append-instruction-sequences
      (spread-arguments operands)
      (make-instruction-sequence '(arg1 arg2) (list target) `((assign ,target (op ,operator) (reg arg1) (reg arg2))))
    )
  )
)
(define (compile-plus exp target linkage) (compile-open-code '+ (operands exp) target linkage))
(define (compile-minus exp target linkage) (compile-open-code '- (operands exp) target linkage))
(define (compile-times exp target linkage) (compile-open-code '* (operands exp) target linkage))
(define (compile-equal exp target linkage) (compile-open-code '= (operands exp) target linkage))

;; d)
(define (compile-open-code-arbi operator operands target linkage)
  (cond
    ((= (length operands) 2) (compile-open-code operator operands target linkage))
    ((and (> (length operands) 2) (memq operator '(+ *)))
     (end-with-linkage
       linkage
       (preserving
         '(env)
         (compile (first-operand operands) 'arg1 'next)
         (preserving
           '(arg1)
           (compile-open-code-arbi operator (rest-operands operands) 'arg2 'next)
           (make-instruction-sequence '(arg1 arg2) (list target) `((assign ,target (op ,operator) (reg arg1) (reg arg2))))
         )
       )
     )
    )
    (else (error "Bad number of arguments -- COMPILE-OPEN-CODE" (list operator operands)))
  )
)
