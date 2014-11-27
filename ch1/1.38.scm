(load "1.37.scm")

; this only gives 2.39, is that series wrong?
; (define (di i)
;   (if (= 0 (remainder (+ i 1) 3))
;     (* 2 (/ (+ i 1) 3))
;     1)
; )

; this gives 2.72
(define (di i)
  (if (= 0 (remainder i 3))
    (* 2 (/ i 3))
    1)
)

(define (e k)
  (+ 2.0 (cont-frac-iter (lambda (i) 1.0) di k))
)

(display (e 100))(newline)
