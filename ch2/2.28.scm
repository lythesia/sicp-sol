(define (reduce op null-val l)
  (if (null? l)
    null-val
    (reduce op (op null-val (car l)) (cdr l))
  )
)

(define (fringe tree)
  (if (pair? tree)
    (reduce append '() (map fringe tree))
    (list tree)
  )
)

; another way
(define (fringe tree)
  (cond 
    ((null? tree) '())
    ((pair? (car tree)) (append (fringe (car tree)) (fringe (cdr tree))))
    (else (cons (car tree) (fringe (cdr tree))))
  )
)

; test
; (define x (list (list '() 1 2) (list 3 4 5 6 (list 7 8))))
; (display x)(newline)
; (display (fringe x))(newline)
; (display (fringe (list x x)))(newline)
