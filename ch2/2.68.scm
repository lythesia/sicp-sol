(load "2.03.04.scm")

(define (encode msg tree)
  (if (null? msg) '()
    (append (encode-symbol (car msg) tree) (encode (cdr msg) tree))
  )
)

(define (encode-symbol sym tree)
  (define (iter result br)
    (if (leaf? br)
      (if (eq? sym (symbol-leaf br))
        result
        (error "bad symbol encounted -- ENCODE-SYMBOL" sym)
      )
      (let ((next (choose-branch-enc sym br)))
        (iter (append result (list (car next))) (cdr next))
      )
    )
  )
  (iter '() tree)
)

(define (choose-branch-enc sym tree)
  (let ((lb (left tree)) (rb (right tree)))
    (if (include-symbol? sym lb)
      (cons 0 lb)
      (cons 1 rb)
    )
  )
)

(define (include-symbol? sym tree)
  (memq sym (symbols tree))
)

; test
; (define tree 
;   (make-code-tree
;     (make-leaf 'A 4)
;     (make-code-tree
;       (make-leaf 'B 2)
;       (make-code-tree
;         (make-leaf 'D 1)
;         (make-leaf 'C 1)
;       )
;     )
;   )
; )
; (define msg '(A D A B B C A))
; (display (encode msg tree))(newline)
