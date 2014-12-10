; `if` only allow one statement
; thus `begin` used to ensure sequential execute
(define (for-each f l)
  (if (not (null? l))
    (begin
      (f (car l))
      (for-each f (cdr l))
    )
  )
)

; `cond` allows multi statements
; since if has `begin` implicitly
(define (for-each f l)
  (cond
    ((not (null? l)) (f (car l)) (for-each f (cdr l)))
  )
)

; test
; (for-each (lambda (x) (display x)(newline)) (list 1 2 3 4))
