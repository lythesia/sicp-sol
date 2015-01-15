(load "3.05.01.stream.scm")
(load "3.50.scm")
(load "3.05.02.is.scm")

(define (merge s1 s2)
  (cond
    ((empty-stream? s1) s2)
    ((empty-stream? s2) s1)
    (else
      (let ((x (stream-car s1)) (y (stream-car s2))) 
        (cond
          ((< x y) (cons-stream x (merge (stream-cdr s1) s2)))
          ((> x y) (cons-stream y (merge s1 (stream-cdr s2))))
          (else (cons-stream x (merge (stream-cdr s1) (stream-cdr s2))))
        )
      )
    )
  )
)

; S: prime multiples only among 2, 3, 5
(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

; test
(display-stream-to S 10)(newline)
