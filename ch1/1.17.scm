(define (double n)
  (+ n n)
)

(define (halve n)
  (/ n 2)
)

(define (multi x y)
  (cond ((= y 0) 0)
        ((even? y) (double (multi x (halve y))))
        (else (+ x (multi x (- y 1))))
  )
)

(display (double 2))(newline)
(display (halve 6))(newline)
(display (multi 2 7))(newline)
(display (multi 3 3))(newline)
