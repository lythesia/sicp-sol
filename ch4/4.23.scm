; take 3 proc as example
; context ver:
(lambda (env)
  (
   (lambda (env-1)
     (proc-1 env-1)
     (proc-2 env-1)
   )
   env
  )
  (proc-3 env)
)

; Alyssa ver:
(lambda (env) (execute-sequence (list proc-1 proc-2 proc-3) env))

;; Alyssa's ver evals procs one-by-one, which does not form the whole sequences into one "eval block", each proc
;; is evaluated by calling execute-sequence indirectly.
;; While context's ver analyze procs into one single "eval block" as an lambda, when fed with env, procs are
;; evaluated sequentially and directly.

;; On efficiency: Alyssa's ver evals with facing `cond` and iterative recursion, which is less efficient than
;; context's ver.
