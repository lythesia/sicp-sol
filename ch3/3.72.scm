(use-modules (ice-9 format))
(load "3.70.scm")

(define (weight-sq-sum p)
  (let ((i (car p)) (j (cadr p)))
    (+ (* i i) (* j j))
  )
)

(define (display-equation p)
  (let ((i (car p)) (j (cadr p)))
    (display (format #t "~d = ~d^2 + ~d^2\n" (weight-sq-sum p) i j))
  )
)

(define (sq-sum-in-3-ways p)
  (let ((first (stream-car p))
        (second (stream-car (stream-cdr p)))
        (third (stream-car (stream-cdr (stream-cdr p)))))
    (if (= (weight-sq-sum first) (weight-sq-sum second) (weight-sq-sum third))
      (begin
        (map display-equation (list first second third))  ; display
        (cons-stream (weight-sq-sum first) (sq-sum-in-3-ways (stream-cdr (stream-cdr (stream-cdr p)))))
      )
      (sq-sum-in-3-ways (stream-cdr p))
    )
  )
)

; test
; (define s (sq-sum-in-3-ways (weighted-pairs integers integers weight-sq-sum)))
; (stream-head s 6)(newline)
