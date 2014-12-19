(load "2.2.3.scm")

; $a_{n} x^{n} + a_{n-1}x^{n-1} + .. + a_{0} = 
; ((a_{n}x + a_{n-1})x + .. + a_{1})x + a_{0}$
(define (horner-eval x coe-seq)
  (accumulate
    (lambda (coe rest-terms) (+ coe (* x rest-terms)))
    0
    coe-seq
  )
)

; test
; (display (horner-eval 2 (list 1 3 0 5 0 1)))(newline) ; => 79
