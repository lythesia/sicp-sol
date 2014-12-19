(load "util.scm")
(load "2.2.3.scm")

(define (map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) '() seq)
)

(define (append s1 s2)
  (accumulate cons s1 s2)
)

(define (length seq)
  ; (accumulate (lambda (x y) (+ 1 y)) 0 seq) ; both ok
  (accumulate (lambda (x y) (1+ y)) 0 seq)
)

; test
; (display (map square '(1 2 3)))(newline)
; (display (append '() '(1 2)))(newline)
; (display (length '(1 2 3 4)))(newline)
