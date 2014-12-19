(define (fold-right op null-val seq)
  (if (null? seq)
    null-val
    (op (car seq) (fold-right op null-val (cdr seq)))
  )
)

(define (fold-left op null-val seq)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))
    )
  )
  (iter null-val seq)
)

(define (reverse-l seq)
  (fold-right
    (lambda (x y) (append y (list x)))
    '()
    seq
  )
)

; test
; (display (reverse-l '(1 2 3 4)))(newline)

(define (reverse-r seq)
  (fold-left
    (lambda (x y) (cons y x))
    '()
    seq
  )
)
; (display (reverse-r '(1 2 3 4)))(newline)
