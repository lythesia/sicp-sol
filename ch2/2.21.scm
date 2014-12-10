(load "util.scm")

; handed
(define (square-list l)
  (if (null? l)
    '()
    (cons (square (car l)) (square-list (cdr l)))
  )
)

; built-in map
(define (square-list l)
  (map square l)
)

; test
; (display (square-list (list 1 2 3 4)))(newline)
