(define (compile-variable exp target linkage compile-time-env)
  (let*
    ((lex-addr (find-variable exp compile-time-env))
     (code
       (if (eq? lex-addr 'not-found)
         `((assign ,target (op lookup-variable-value) (const ,exp) (reg env)))
         `((assign ,target (op lexical-address-lookup) (const ,lex-addr) (reg env)))
       )))
    (end-with-linkage linkage (make-instruction-sequence '(env) (list target) code))
  )
)

(define (compile-assignment exp target linkage compile-time-env)
  (let* ((var (assignment-variable exp)) (get-value-code (compile (assignment-value exp) 'val 'next)) (lex-addr (find-variable var compile-time-env)))
    (end-with-linkage
      linkage
      (preserving
        '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
          (if (eq? lex-addr 'not-found)
            `((perform (op set-variable-value!) (const ,var) (reg val) (reg env))
              (assign ,target (const ok)))
            `((perform (op lexical-address-set!) (const ,lex-addr) (reg env) (reg val))
              (assign ,target (const ok)))
          )
        )
      )
    )
  )
)
