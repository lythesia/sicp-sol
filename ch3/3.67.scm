(load "3.05.01.stream.scm")
(load "3.05.02.is.scm")

(define (pairs-all s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    ; (interleave-s
    ;   (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    ;   (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s))
    ;   (pairs (stream-cdr s) (stream-cdr t))
    ; )
    (interleave
      (interleave
        (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
        (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s))
      )
      (pairs-all (stream-cdr s) (stream-cdr t))
    )
  )
)

; test
; (define x (pairs-all integers integers))
; (stream-head x 30)(newline)
