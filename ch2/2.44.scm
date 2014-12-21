(define (up-split p n)
  (if (= n 0) p
    (let ((smaller (up-split p (1- n))))
      (below p (beside smaller smaller))
    )
  )
)
