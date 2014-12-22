(load "2.59.scm")

; no need modify:
;   element-of-set?
;   intersection-set

(define (adjoin-set x set) (cons x set))

(define (union-set s1 s2) (append s1 s2))

; test
; (define s1 '( 1 2 3 2 3 3 ))
; (define s2 '( 3 4 3 5 6 6 ))
; (display (element-of-set? '() s1))(newline)
; (display (element-of-set? 1 s1))(newline)
; (display (adjoin-set 0 s1))(newline)
; (display (adjoin-set 1 s1))(newline)
; (display (intersection-set s1 s2))(newline)
; (display (union-set s1 s2))(newline)
