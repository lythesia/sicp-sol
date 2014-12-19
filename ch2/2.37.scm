(load "2.36.scm")

(define (dot-product v w)
  (accumulate + 0 (map * v w))
)

(define (matrix-*-vector m v)
  (map
    (lambda (row) (dot-product row v))
    m
  )
)

(define (transpose m)
  (accumulate-n cons '() m)
)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
      (lambda (row) (matrix-*-vector cols row))
      m
    )
  )
)

; test
; (define m (list '(1 2 3 4) '(4 5 6 6) '(6 7 8 9)))
; (define v '(1 2 3 4))
; (display (matrix-*-vector m v))(newline)
; (display (transpose m))(newline)
; (display (matrix-*-matrix m (transpose m)))(newline)
