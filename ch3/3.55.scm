(load "3.05.01.stream.scm")
(load "3.50.scm")
(load "3.05.02.is.scm")

(define (partial-sums s)
  ; put (partial-sums s) after since:
  ; add-streams base on stream-map, which terminates on first stream
  ; so the first stream should be **SHORTEST**, consider if s is finite stream.
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s)))
)

; test
; (define psi (partial-sums integers))
; (display (stream-ref psi 4))(newline) ; => 15
