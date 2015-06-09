(define dont-run-any 1)
(include "4.33.scm")

; preserve primitive scheme cons/car/cdr
(map
  (lambda (name proc) (define-variable! name (list 'primitive proc) the-global-environment))
  (list 'scm-cons 'scm-car 'scm-cdr)
  (list cons car cdr)
)

; install tagged ones into env
(actual-value
  '(begin
     (define (cons x y)
       (scm-cons 'cons (lambda (m) (m x y)))
     )
     (define (car z)
       ((scm-cdr z) (lambda (p q) p))
     )
     (define (cdr z)
       ((scm-cdr z) (lambda (p q) q))
     )
  )
  the-global-environment
)

(define (disp-cons obj depth)
  (letrec
    ((user-car (lambda (z) (force-it (lookup-variable-value 'x (procedure-environment (cdr z))))))
     (user-cdr (lambda (z) (force-it (lookup-variable-value 'y (procedure-environment (cdr z)))))))
    (cond
      ((>= depth 10) (display "...)"))
      ((null? obj) (display ""))
      (else
        (let ((cdr-val (user-cdr obj)))
          (display "(")
          (display (user-car obj))
          (if (tagged-list? cdr-val 'cons)
            (begin
              (display " ")
              (disp-cons cdr-val (+ depth 1))
            )
            (begin
              (display " . ")
              (display cdr-val)
            )
          )
          (display ")")
        )
      )
    )
  )
)

(define (user-print obj)
  (if (compound-procedure? obj)
    (display
      (list
        'compound-procedure
        (procedure-parameters obj)
        (procedure-body obj)
        '<procedure-env>
      )
    )
    (if (tagged-list? obj 'cons)
      (disp-cons obj 0)
      (display obj)
    )
  )
)
(driver-loop)
