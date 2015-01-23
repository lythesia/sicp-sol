(load "3.73.integral.scm")

(define (RC R C dt)
  (lambda (i v0)
    (add-streams
      (scale-stream i R)
      (integral (scale-stream i (/ 1.0 C)) v0 dt)
    )
  )
)

; test
; (define RC1 (RC 5 1 0.5))
; (define rc-series (RC1 ones 2.0))
; (stream-head rc-series 5)(newline)
