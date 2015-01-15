(load "3.05.01.stream.scm")
(load "3.50.scm")
(load "3.05.02.is.scm")

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams factorials (integers-starting-from 2)))
)

; test
; (display (stream-ref factorials 2))(newline) ; => 6
