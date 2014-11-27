(define (sq x)
  (* x x)
)

(define (sum-of-sq x y)
  (+ (sq x) (sq y))
)

(define (mmax x y)
  (if (> x y) x y)
)

(define (mmin x y)
  (if (> x y) y x)
)

;(define max1 (mmax x y))
;(define max2 (mmax (mmin x y) z))

(define (foo x y z)
  (sum-of-sq (mmax x y) (mmax (mmin x y) z))
)

; naive
;(define (foo x y z)
;  (if (> x y)
;    (if (> z y)
;      (sum-of-sq x z)
;      (sum-of-sq x y))
;    (if (> z x)
;      (sum-of-sq y z)
;      (sum-of-sq y x))
;  )
;)

(display (foo 1 2 3))(newline)
(display (foo 1 3 2))(newline)
(display (foo 2 1 3))(newline)
(display (foo 2 3 1))(newline)
(display (foo 3 1 2))(newline)
(display (foo 3 2 1))(newline)
