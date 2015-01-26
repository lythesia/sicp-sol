(load "3.05.01.stream.scm")
(load "3.50.scm")
(load "3.05.02.is.scm")
(load "3.05.04.delay-int.scm")

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y
)

; test
; (define yy (solve-2nd 2 3 0.001 2 2))
; (display (stream-ref yy 1000))(newline)
