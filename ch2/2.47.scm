(load "2.46.scm")

; ver 1
(define (make-frame o e1 e2) (list o e1 e2))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

; ver 2
(define (make-frame o e1 e2) (cons o (cons e1 e2)))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (cddr f))

; test
; (define f1 (make-frame (cons 0 0) (cons 1 2) (cons 3 4)))
; (display (origin-frame f1))(newline)
; (display (edge1-frame f1))(newline)
; (display (edge2-frame f1))(newline)
