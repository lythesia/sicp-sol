(load "2.2.3.scm")

(define (count-leaves t)
  (accumulate
    +
    0
    (map 
      (lambda (sub-tree) (if (pair? sub-tree) (count-leaves sub-tree) 1))
      t
    )
  )
)

; test
; (display (count-leaves (list (list 1 2) (list 3 4))))(newline)
; (display (count-leaves (list (list 1 (list 2 3)) (list (list 4 5) (list 6 7)))))(newline)
