(load "3.75.scm")

(define (smooth s)
  (let* ((fst (stream-car s)) (snd (stream-car (stream-cdr s))) (avg (/ (+ fst snd) 2)))
    (cons-stream avg (smooth (stream-cdr s)))
  )
)

; another
; (define (smooth s)
;   (stream-map (lambda (a b) (/ (+ a b) 2)) s (stream-cdr s))
; )

(define smoothed-input (smooth sense-data))
(define zero-crossings (stream-map sign-change-detector (smoothed-input) (cons-stream 0 smoothed-input)))
