(define (double n)
  (+ n n)
)

(define (halve n)
  (/ n 2)
)

(define (multi x y)
  (multi-iter x y 0)
)

(define (multi-iter x y a)
  (cond ((= y 0) a)
        ((even? y) (multi-iter (double x) (halve y) a))
        (else (multi-iter x (- y 1) (+ a x)))
  )
)

(display (multi 2 7))(newline)
