; ordered
(define (adjoin-set x set)
  (define (iter result rest)
    (cond
      ((null? rest) (append result (list x)))
      ((= x (car rest)) set)
      ((< x (car rest)) (append result (cons x rest)))
      (else (iter (append result (list (car rest))) (cdr rest)))
    )
  )
  (iter '() set)
)

; test
; (define s1 '(1 3 5 7))
; (display (adjoin-set 0 s1))(newline)
; (display (adjoin-set 4 s1))(newline)
; (display (adjoin-set 9 s1))(newline)
; (display (adjoin-set 3 s1))(newline)
