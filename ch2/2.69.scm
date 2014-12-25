(load "2.03.04.scm")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))
)

(define (successive-merge tree)
  (if (null? (cdr tree))
    (car tree)
    (let* ((fis (car tree)) (sec (cadr tree)) (rest (cddr tree)) (new (make-code-tree fis sec)))
      (successive-merge (adjoin-set new rest))
    )
  )
)

; test
; (define pairs '((A 4) (B 2) (C 1) (D 1) ))
; (define tree (generate-huffman-tree pairs)) ; gen tree diff with book, since C D both 1
; (display tree)(newline)
; (load "2.68.scm")
; (define msg '(A D A B B C A))
; (display (encode msg tree))(newline)
