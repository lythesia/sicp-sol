(define (mystery x)
  (define (loop x y)
    (if (null? x) y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x)
      )
    )
  )
  (loop x '())
)

; reverse x (act like memmov, afterly x point to single element)
; test
; (define v (list 1 2 3))
; (display v)(newline)
; (define w (mystery v))
; (display w)(newline)
; (display v)(newline)
