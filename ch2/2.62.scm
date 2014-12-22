; ordered
(define (union-set s1 s2)
  (if (or (null? s1) (null? s2))
    (if (null? s1) s2 s1)
    (let ((x1 (car s1)) (x2 (car s2)) (r1 (cdr s1)) (r2 (cdr s2)))
      (cond
        ((= x1 x2) (cons x1 (union-set r1 r2)))
        ((< x1 x2) (cons x1 (union-set r1 s2)))
        (else (cons x2 (union-set s1 r2)))
      )
    )
  )
)

; test
; (define s1 '(1 3 5))
; (define s2 '(1 2 6))
; (display (union-set s1 s2))(newline)
