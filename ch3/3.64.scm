(load "util.scm") ; for average
(load "3.05.01.stream.scm")
(load "3.50.scm")
(load "3.05.02.is.scm")
(load "3.05.03.sqrt.scm") ; for sqrt-stream

(define (stream-limit s eps)
  (let ((x (stream-car s)) (r (stream-cdr s)))
    (cond
      ((empty-stream? r) x) ; fallback for finite stream
      ((< (abs (- (stream-car s) (stream-car r))) eps) (stream-car r))
      (else (stream-limit r eps))
    )
  )
)

(define (sqrt-eps x eps)
  (stream-limit (sqrt-stream x) eps)
)

; test
; (display (sqrt-eps 2 0.0001))(newline)
