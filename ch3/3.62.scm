(load "3.61.scm")

; this is wrong, since s2 may not start with 1
; (mul-series s1 (inverse-series s2))

; div 0!
(define (div-series s1 s2)
  (let ((den (stream-car s2)))
    (if (zero? den)
      (error "costant term 0! -- DIV-SERIES" den)
      (scale-stream
        (mul-series
          s1
          (inverse-series (scale-stream s2 (/ 1 den))) ; ensure inv 1 ...
        )
        (/ 1 den)
      )
    )
  )
)

; test
; (define tangent-series (div-series sine-series cosine-series))
; (display-stream-to tangent-series 8) ; 0 1 0 1/3 0 2/15 0 17/315 ...
