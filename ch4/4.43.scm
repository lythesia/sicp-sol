(define (oyago)
  ; (otousan musume)
  (let
    ((Moore 'Mary)
     (Barnnacle 'Melissa)
     (Hall (amb 'Lorna 'Gabrelle))
     (Downing (amb 'Lorna 'Gabrelle 'Rosalind))
     (Parker (amb 'Lorna 'Rosalind))) ; <- think about it
    (require )
    (require (distinct? (list Moore Barnnacle Hall Downing Parker)))
    (require
      ;; here we only do one-step judge, rest is left for `distinct?`
      (cond
        ((eq? Hall 'Gabrelle) (eq? 'Rosalind Parker))
        ((eq? Downing 'Gabrelle) (eq? 'Melissa Parker)) ; though this is false
        (else #f)
      )
    )
    (require (distinct? (list Moore Barnnacle Hall Downing Parker)))
    (map
      (lambda (otousan musume) (list otousan musume))
      '(Moore Barnnacle Hall Downing Parker)
      (list Moore Barnnacle Hall Downing Parker)
    )
  )
)

;; what if Ann's father is not Moore:
;; there'll be one more solution, (Moore Gabrelle) (Barnacle Melissa) (Hall Mary) (Downing Rosalind) (Parker Lorna)
