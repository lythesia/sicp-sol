(include "5.05.compile.scm")

(define compiled-instruction-sequence
  (compile
    '(define (factorial-alt n)
       (if (= n 1)
         1
         (* n (factorial-alt (- n 1)))))
    'val
    'next
  )
)

(map (lambda (x) (display x)(newline)) (statements compiled-instruction-sequence))

;; for (* (f (- n 1)) n), we process `n` first in arglist, then we save `argl` since there's procedure call ahead (f (- n 1)); but we dont need `env`, since after lookup `n` and (f ..), there's no arg need env

;; for (* n (f (- n 1))), we process (f ..) first in arglist, after procedure call, `argl` set to (list [val]), and `n` ahead will not broken `argl` but directly cons to it; but we need save `env` before (f ..), since this call would modify env, and after that we need original `env` to lookup `n`

;; thus, former push/pop argl, latter push/pop env, there's no efficiency difference.
