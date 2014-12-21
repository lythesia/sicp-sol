; ver 1
(define (split m s)
  (lambda (p n)
    (if (= n 0) p
      (let ((slave ((split m s) p (-1 n)))) ; `((split m s) p (-1 n))` since we cannot refer the lambda self
        (m p (s slave slave))
      )
    )
  )
)

; ver 2
(define (Y f)
  ((lambda (u) (u u))
   (lambda (x) (f (lambda args (apply (x x) args))))
  )
)

; utilizing Y
(define (split m s)
  (Y 
    (lambda (sp)
      (lambda (p n)
        (if (= n 0) p
          (let ((slave (sp p (-1 n))))
            (m p (s slave slave))
          )
        )
      )
    )
  )
)
; test Y
; (define fac (Y (lambda (f) (lambda (n) (if (zero? n) 1 (* n (f (1- n))))))))
; (display (fac 10))(newline)
