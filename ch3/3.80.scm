(load "3.78.scm")

(define (RLC R L C dt)
  (lambda (vc0 il0)
    ; must define in this order!
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1.0 C)))
    (define dil (add-streams (scale-stream vc (/ 1.0 L)) (scale-stream il (- (/ R L)))))
    (stream-map cons vc il)
  )
)

; test
; (define RLC0 (RLC 1 0.2 1 0.1))
; (define RLC0-series (RLC0 10 0))
; (stream-head RLC0-series 10)(newline)
