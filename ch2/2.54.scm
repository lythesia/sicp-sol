(define (nil-non-nil? l1 l2) (and (null? l1) (not (null? l2))))
(define (equal? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (nil-non-nil? l1 l2) (nil-non-nil? l2 l1)) #f)
    (else (if (eqv? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)) #f))
  )
)

(define (equal? l1 l2)
  (if (and (pair? l1) (pair? l2))
    (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))
    (eqv? l1 l2)
  )
)

; tips:
; equal?: iff eqv?, unless specified for particular datatype
; eqv?:   iff eq?, unless .., e.g: number, character
; eq?:    same object

; test
; (display (equal? '(this is a list) '(this is a list)))(newline)
; (display (equal? '(this is a list) '(this (is a) list)))(newline)
; (display (equal? (list 1 2 3) (list 1 2 4)))(newline)
