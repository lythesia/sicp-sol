(define (smallest-divisor n)
  (find-divisor n 2)
)

(define (find-divisor n test)
  (cond ((> (square test) n) n)
        ((divides? n test) test)
        (else (find-divisor n (+ test 1)))
  )
)

(define (divides? n test)
  (= (remainder n test) 0)
)

(define (square n)
  (* n n)
)

; (display (smallest-divisor 200))(newline)
; (display (smallest-divisor 1999))(newline)
; (display (smallest-divisor 19999))(newline)
