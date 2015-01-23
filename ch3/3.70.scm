(load "3.05.01.stream.scm")
(load "3.05.02.is.scm")

(define (merge-weighted s1 s2 weight)
  (cond
    ((empty-stream? s1) s2)
    ((empty-stream? s2) s1)
    (else
      (let* ((x (stream-car s1)) (y (stream-car s2)) (wx (weight x)) (wy (weight y)))
        (cond
          ((< wx wy) (cons-stream x (merge-weighted (stream-cdr s1) s2 weight)))
          (else (cons-stream y (merge-weighted s1 (stream-cdr s2) weight)))
        )
      )
    )
  )
)

(define (weighted-pairs s t w)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) w)
      w
    )
  )
)

(define (weight-sum p) (apply + p))
; (define weight-sum-pairs (weighted-pairs integers integers weight-sum))

(define (weight-235 p)
  (let ((i (car p)) (j (cadr p)))
    (+ (* 2 i) (* 3 j) (* 5 i j))
  )
)
(define i-235
  (stream-filter (lambda (x) (and (divisible? x 2) (divisible? x 3) (divisible? x 5))) integers)
)
; (define weight-235-pairs (weighted-pairs i-235 i-235 weight-235))

; test
; (stream-head weight-sum-pairs 10)(newline)
; (stream-head weight-235-pairs 10)(newline)
