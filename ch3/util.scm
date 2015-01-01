(define (square x) (expt x 2))

(define (make-cycle x) (set-cdr! (last-pair x) x) x)
