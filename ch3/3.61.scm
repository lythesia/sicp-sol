(load "3.60.scm")

; s must start with 1
(define (inverse-series s)
  (cons-stream
    1
    (mul-streams
      neg-ones
      (mul-series (stream-cdr s) (inverse-series s))
    )
  )
)

; test
; (define i-cosine-series (inverse-series cosine-series))
; (define cic-series (mul-series cosine-series i-cosine-series))
; (display-stream-to cic-series 5)(newline)
