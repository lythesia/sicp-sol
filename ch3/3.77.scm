(load "3.05.01.stream.scm")
(load "3.50.scm")
(load "3.05.02.is.scm")

; self ref
; (define (integral delayed-integrand initial-value dt)
;   (define int
;     (cons-stream
;       initial-value
;       (let ((integrand (force delayed-integrand)))
;         (add-streams (scale-stream integrand dt) int)
;       )
;     )
;   )
;   int
; )

; integral-starting-from like
(define (integral delayed-integrand initial-value dt)
  (cons-stream
    initial-value
    (let ((integrand (force delayed-integrand)))
      (if (empty-stream? integrand)
        the-empty-stream
        (integral
          (delay (stream-cdr integrand))
          (+ (* (stream-car integrand) dt) initial-value)
          dt
        )
      )
    )
  )
)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y
)

; test
; (display (stream-ref (solve (lambda (y) y) 1 0.001) 1000))(newline)
