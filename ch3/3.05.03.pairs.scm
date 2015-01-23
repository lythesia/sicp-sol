(load "3.05.01.stream.scm")
(load "3.05.02.is.scm")

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t))
    )
  )
)

; test
; (define x (pairs integers integers))
; (stream-head x 12)(newline)
