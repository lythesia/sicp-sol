(define (fixed-point f x eps)
  (define (close-enough? x y)
    (< (abs (- x y)) eps)
  )

  (define (try x)
    ; (display x)(newline)
    (let ((next (f x)))
      (if (close-enough? next x) next (try next))
    )
  )

  (try x)
)

(define (log-x-1000)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0 0.00001)
)

; (log-x-1000)
