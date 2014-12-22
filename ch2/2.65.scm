; bst
(define (entry tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))
(define (make-tree v l r) (list v l r))

;     o
;   /   \
;  l ... r          ()
;
;  to
;
;     o
;   /   \
;  l ..r'nil        (r)
;
; ..
;
; right-root-left   expand to left
;     l             ( o .. r )
;    / \
;   .....
(define (tree->list tree)
  (define (iter result rest-tree)
    (if (null? rest-tree)
      result
      (iter (cons (entry rest-tree) (iter result (right rest-tree))) (left rest-tree))
    )
  )
  (iter '() tree)
)

; bst -> list
(define (list->tree elements)
  (define (partial-tree elts n)
    (if (= n 0)
      (cons '() elts)
      (let*
        (; left
         (l-size (quotient (1- n) 2))
         (l-result (partial-tree elts l-size))
         (l-tree (car l-result))
         (non-l (cdr l-result))
         ; current
         (this (car non-l))
         ; right
         (r-size (- (1- n) l-size))
         (r-result (partial-tree (cdr non-l) r-size))
         (r-tree (car r-result))
         (rem (cdr r-result))
        )
        (cons (make-tree this l-tree r-tree) rem)
      )
    )
  )
  (car (partial-tree elements (length elements)))
)

; test
; (define t '(5 (3 () (1 () ())) (9 (7 () ()) (11 () ()))))
; (display t)(newline)
; (define t-l (tree->list t))
; (display t-l)(newline)
; (define t-l-t (list->tree t-l))
; (display t-l-t)(newline)


(load "2.62.scm") ; for intersection-set, union-set
(define (intersection-set-tree t1 t2)
  (let ((s1 (tree->list t1)) (s2 (tree->list t2)))
    (list->tree (intersection-set s1 s2))
  )
)

(define (union-set-tree t1 t2)
  (let ((s1 (tree->list t1)) (s2 (tree->list t2)))
    (list->tree (union-set s1 s2))
  )
)
