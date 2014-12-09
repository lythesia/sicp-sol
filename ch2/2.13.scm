(load "2.12.scm")

; (x + dx)(y + dy)
; = xy(1 + dx/x)(1 + dy/y)
; = xy(1 + (dx/x + dy/y))
(define (mul-interval x y)
  (make-center-percent (* (center x) (center y)) (+ (percent x) (percent y)))
)
