(define (compile-lambda-body exp proc-entry compile-time-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
      (make-instruction-sequence '(env proc argl) '(env)
        `(,proc-entry
           (assign env (op compiled-procedure-env) (reg proc))
           (assign env (op extend-environment) (const ,formals) (reg argl) (reg env))
         )
      )
      ; scan-out-defines from ch4/4.16.scm
      ; we should support let here since scan-out-defines used
      (compile-sequence (scan-out-defines (lambda-body exp)) 'val 'return (cons formals compile-time-env))
    )
  )
)
