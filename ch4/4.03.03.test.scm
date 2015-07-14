(define (require p)
  (if (not p) (amb))
)

(define (distinct? lst)
  (cond
    ((null? lst) true)
    ((null? (cdr lst)) true)
    ((member (car lst) (cdr lst)) false)
    (else (distinct? (cdr lst)))
  )
)

(define (oyago)
  ; (otousan musume)
  (let
    ((Moore 'Mary)
     (Barnacle 'Melissa)
     (Hall (amb 'Lorna 'Gabrelle))
     (Downing (amb 'Lorna 'Gabrelle 'Rosalind))
     (Parker (amb 'Lorna 'Rosalind))) ; <- think about it
    (require (distinct? (list Moore Barnacle Hall Downing Parker)))
    (require
      ;; here we only do one-step judge, rest is left for `distinct?`
      (cond
        ((eq? Hall 'Gabrelle) (eq? 'Rosalind Parker))
        ((eq? Downing 'Gabrelle) (eq? 'Melissa Parker)) ; though this is false
        (else false)
      )
    )
    (require (distinct? (list Moore Barnacle Hall Downing Parker)))
    (list
      (list 'Barnacle Barnacle)
      (list 'Moore Moore)
      (list 'Hall Hall)
      (list 'Downing Downing)
      (list 'Parker Parker)
    )
  )
)

(oyago)
