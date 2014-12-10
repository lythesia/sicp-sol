; recursive
(define (reverse l)
  (cond
    ((null? l) (error "empty list"))
    ((null? (cdr l)) l)
    (else (append (reverse (cdr l)) (list (car l))))
  )
)

; iterative
(define (reverse-iter l)
  (define (iter rest result)
    (if (null? rest)
      result
      (iter (cdr rest) (cons (car rest) result))
    )
  )
  (iter l '())
)

; (define rl (reverse-iter (list 1 2 3 4)))
; (display rl)(newline)
; (display (list-ref rl 3))(newline)
