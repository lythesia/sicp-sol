(load "2.8.scm")

(define (cover-zero? i) (and (<= (lower-bound i) 0) (>= (upper-bound i) 0)))
(define (div-interval x y)
  (if (cover-zero? y) (error "div 0!")
    (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))
  )
)

; (define i1 (make-interval 1 10))
; (define i2 (make-interval -2 5))

; (display i1)(newline)
; (display i2)(newline)
; (display (div-interval i1 i2))(newline)
