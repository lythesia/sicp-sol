(define (find-variable var compile-time-env)
  (define (env-loop envs e)
    (define (scan vars v)
      (cond
        ((null? vars) (env-loop (cdr envs) (+ e 1)))
        ((eq? var (car vars)) (list e v))
        (else (scan (cdr vars) (+ v 1)))
      )
    )
    (if (null? envs)
      'not-found
      (scan (car envs) 0)
    )
  )
  (env-loop compile-time-env 0)
)

;; test
; (define envs '((y z) (a b c d e) (x y)))
; (display (find-variable 'c envs))(newline)
; (display (find-variable 'x envs))(newline)
; (display (find-variable 'w envs))(newline)
