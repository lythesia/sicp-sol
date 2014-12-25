; Huffman

(define (make-leaf sym weight)
  (list 'leaf sym weight) ; mark as 'leaf
)
(define (leaf? node) (eq? (car node) 'leaf))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

; tree -> (left, right, all-sym-set, weight)
; e.g:
;   (A B C D) 17
;      / \
;    /    \
;   A 8   (B C D) 9
;  ...
(define (make-code-tree left right)
  (list
    left
    right
    (append (symbols left) (symbols right))
    (+ (weight left) (weight right))
  )
)
(define (left tree) (car tree))
(define (right tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)
  )
)
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)
  )
)

(define (make-map-pair s w) (list s w))
(define (genmap tree)
  (if (null? tree) '()
    (cond
      ((leaf? tree) (list (make-map-pair (symbol-leaf tree) (weight-leaf tree))))
      (else (append (genmap (left tree)) (genmap (right tree))))
    )
  )
)

; decode [target-bit-string] [huff-tree]
(define (decode bits tree)
  (define (decode-l bits current)
    (if (null? bits)
      '()
      (let* ((bit (car bits)) (rest (cdr bits)) (next-branch (choose-branch bit current)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch) (decode-l rest tree))
          (decode-l rest next-branch)
        )
      )
    )
  )
  (decode-l bits tree)
)
; next-branch helper
(define (choose-branch bit tree)
  (cond
    ((= bit 0) (left tree))
    ((= bit 1) (right tree))
    (else (error "bad bit -- CHOOSE-BRANCH" bit))
  )
)

; set with weight (ordered list, NOT bst), asc
(define (adjoin-set x set)
  (if (null? set) (list x)
    (let ((cur (car set)))
      (if (< (weight x) (weight cur))
        (cons x set)
        (cons cur (adjoin-set x (cdr set)))
      )
    )
  )
)
; pairs: ('sym, weight)
(define (make-leaf-set pairs)
  (if (null? pairs) '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) (cadr pair)) (make-leaf-set (cdr pairs)))
    )
  )
)
