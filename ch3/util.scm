(define (square x) (expt x 2))

(define (make-cycle x) (set-cdr! (last-pair x) x) x)

(define (for-each-except e proc l)
  (define (loop items)
    (cond
      ((null? items) 'done)
      ((eq? (car items) e) (loop (cdr items)))
      (else (proc (car items)) (loop (cdr items)))
    )
  )
  (loop l)
)
