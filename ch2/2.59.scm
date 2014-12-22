(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((equal? x (car set)) #t)
    (else (element-of-set? x (cdr set)))
  )
)

(define (adjoin-set x set)
  (if (element-of-set? x set) set (cons x set))
)

(define (intersection-set s1 s2)
  (if (or (null? s1) (null? s2)) '()
    (let ((x (car s1)) (result (intersection-set (cdr s1) s2)))
      (if (element-of-set? x s2) (cons x result) result)
    )
  )
)

(define (union-set s1 s2)
  (if (null? s2) s1 (union-set (adjoin-set (car s2) s1) (cdr s2)))
)

; test
; (define s1 '(1 2 3))
; (define s2 '(3 4 5))
; (display (element-of-set? '() s1))(newline)
; (display (element-of-set? 1 s1))(newline)
; (display (adjoin-set 0 s1))(newline)
; (display (adjoin-set 1 s1))(newline)
; (display (intersection-set s1 s2))(newline)
; (display (union-set s1 s2))(newline)
