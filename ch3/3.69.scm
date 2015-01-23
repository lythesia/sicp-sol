(load "util.scm")
(load "3.05.01.stream.scm")
(load "3.05.02.is.scm")
(load "3.05.03.pairs.scm")

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (tu) (cons (stream-car s) tu)) (pairs t u))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))
    )
  )
)

(define (ok? i j k) (= (+ (* i i) (* j j)) (* k k)))

(define phythagoras-triples
  (stream-filter (lambda (stu) (apply ok? stu)) (triples integers integers integers))
)

; test
; really slow!
(stream-head phythagoras-triples 4)(newline)
