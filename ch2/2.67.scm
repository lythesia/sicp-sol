(load "2.03.04.scm")

(define tree 
  (make-code-tree
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree
        (make-leaf 'D 1)
        (make-leaf 'C 1)
      )
    )
  )
)

(define msg '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display (decode msg tree))(newline)
