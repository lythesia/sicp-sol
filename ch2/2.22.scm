; (cons list e) -> (list . e)
; (cons e list) -> (e list)

(load "util.scm")
(load "2.18.scm")

(define (square-list l)
  (define (iter rest result)
    (if (null? rest)
      result
      (iter (cdr rest) (cons (square (car rest)) result))
    )
  )
  (iter (reverse l) '())
)

; test
; (display (square-list (list 1 2 3 4)))(newline)
