(load "3.59.scm")

(define (mul-series s1 s2)
  (cons-stream
    (* (stream-car s1) (stream-car s2))
    (add-streams
      (add-streams
        (scale-stream (stream-cdr s1) (stream-car s2))  ; b0 exp: 1 2 ... n ...
        (scale-stream (stream-cdr s2) (stream-car s1))  ; a0 exp: 1 2 ... n ...
      )
      ; exp: 1 (x^1 computed in above 2 line, so coe = 0), mul-series will gen 2 3 ... n ...
      (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))
    )
  )
)

; test
(define s2-plus-c2
  (add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series))
)
; (display-stream-to s2-plus-c2 5)(newline)
