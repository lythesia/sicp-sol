(define (last-pair l)
  (cond 
    ((null? l) (error "empty list"))
    ((null? (cdr l)) (car l))
    (else (last-pair (cdr l)))
  )
)

; test
; (display (last-pair (list 1)))(newline)
; (display (last-pair (list 1 2 3 4)))(newline)
; (display (last-pair (list)))(newline)
