(load "util.scm")

(define (square-tree tree)
  (cond
    ((null? tree) '())
    ((not (pair? tree)) (square tree))
    (else
      (cons (square-tree (car tree)) (square-tree (cdr tree)))
    )
  )
)

(define (square-tree tree)
  (map
    (lambda (sub-tree)
      (if (pair? sub-tree)
        (square-tree sub-tree)
        (square sub-tree)
      )
    )
    tree
  )
)

; test
; (define t (list 1 (list 2 (list 3 4) 5)))
; (display t)(newline)
; (display (square-tree t))(newline)
