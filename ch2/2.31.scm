(load "util.scm")

(define (tree-map op tree)
  (map
    (lambda (sub-tree)
      (if (pair? sub-tree)
        (tree-map op sub-tree)
        (op sub-tree)
      )
    )
    tree
  )
)
(define (square-tree tree) (tree-map square tree))

; or currying
(define (tree-map op)
  (lambda (tree)
    (map 
      (lambda (sub-tree) 
        (if (pair? sub-tree) 
          ((tree-map op) sub-tree) 
          (op sub-tree)
        )
      )
      tree
    )
  )
)
(define square-tree (tree-map square))

; test
; (define t (list 1 (list 2 (list 3 4) 5)))
; (display (square-tree t))(newline)
