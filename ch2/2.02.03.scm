(define (enumerate-interval m n)
  (if (> m n)
    '()
    (cons m (enumerate-interval (1+ m) n))
  )
)

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

; (map proc seq) => generate new seq from seq
; (accumulate append ..) => append into list
(define (flatmap proc seq)
  (accumulate append '() (map proc seq))
)

; e.g. permutation
(define (remove t seq)
  (filter (lambda (x) (not (= t x))) seq)
)

(define (permutations s)
  (if (null? s)
    (list '())
    (flatmap
      (lambda (x)
        (map (lambda (p) (cons x p)) (permutations (remove x s)))
      )
      s
    )
  )
)
