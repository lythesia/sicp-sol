(define (unbound? exp) (tagged-list? exp 'unbound!))
(define (unbound-variable exp) (cadr exp))
; only unbound in current frame
;   focus on next because `set-cdr!` used
(define (make-unbound! var env)
  (let ((f (first-frame)))
    (define (scan vars vals)
      (let ((n-vars (cdr vars)) (n-vals (cdr vals)))
        (cond
          ((null? n-vars) 'done)
          ((eq? var (car (n-vars))) (set-cdr! vars (cdr n-vars)) (set-cdr! vals (cdr n-vals)))
          (else (scan n-vars n-vals))
        )
      )
    )
    (let ((vars (frame-variables f)) (vals (frame-values f)))
      (if (null? vars)
        'done
        (if (eq? var (car vars))
          (begin
            (set-car! f (cdr vars))
            (set-cdr! f (cdr vals))
          )
          (scan vars vals)
        )
      )
    )
  )
)
