(define (make-operation-exp exp machine lables operations)
  (let
    ((op (lookup-operation (operation-exp-op exp) operations))
     (a-procs
       (map
         (lambda (e)
           (if (label-exp? e)
             (error "cannot operate on label -- MAKE-OPERATION-EXP" e)
             (make-primitive-exp e machine labels))
           )
         (operation-exp-operands exp)
       )
     ))
    (lambda () (apply op (map (lambda (p) (p)) a-procs)))
  )
)
