; later define is ok

(define (sqrtx x)
  (sqrt-iter 1.0 x)
)

(define (sqrt-iter guess x)
  (if (conv? guess (improve guess x))
    (improve guess x)
    (sqrt-iter (improve guess x) x)
  )
)

; or avoid repeat (improve guess x) via let
; (define (sqrt-iter old-guess x)
    ; (let ((new-guess (improve old-guess x)))
        ; (if (good-enough? old-guess new-guess)
            ; new-guess
            ; (sqrt-iter new-guess x))))

(define (improve x y)
  (average x (/ y x))
)

(define (average x y)
  (/ (+ x y) 2)
)

(define (conv? old-guess new-guess)
  (> 0.01 (/ (abs(- new-guess old-guess)) old-guess))
)

(display (sqrtx 9))(newline)
(display (sqrt 900000000000000000000000000000000000000000000000000000000000000000000000000000000000))(newline)
