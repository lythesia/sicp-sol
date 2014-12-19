(define (filter pred seq)
  (cond
    ((null? seq) '())
    ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
    (else (filter pred (cdr seq)))
  )
)

(define (accumulate op null-val seq)
  (if (null? seq)
    null-val
    (op (car seq) (accumulate op null-val (cdr seq)))
  )
)
