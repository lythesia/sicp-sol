; bst
(define (make-tree v l r) (list v l r))
(define (entry node) (car node))
(define (left node) (cadr node))
(define (right node) (caddr node))

(define (key-tree node) (car node))
(define (val-tree node) (cdr node))

(define (set-left! tree new-left) (set-car! (cdr tree) new-left))
(define (set-right! tree new-right) (set-car! (cddr tree) new-right))

(define (lookup-tree key tree comp)
  (if (null? tree) #f
    (let* ((record (entry tree)) (comp-result (comp key (key-tree record))))
      (cond
        ; equal
        ((= 0 comp-result) record)
        ; key > current
        ((> 0 comp-result) (lookup-tree key (right tree) comp))
        ; key < current
        (else (lookup-tree key (left tree) comp))
      )
    )
  )
)

(define (insert-tree! node tree comp)
  (if (null? tree)
    (make-tree node '() '())
    (let* ((k (key-tree node)) (v (val-tree node)) (cur (entry tree)) (comp-result (comp k (key-tree cur))))
      (cond
        ((= 0 comp-result) (set-cdr! cur v) tree)
        ((> 0 comp-result) (set-right! tree (insert-tree! node (right tree) comp)) tree)
        (else (set-left! tree (insert-tree! node (left tree) comp)) tree)
      )
    )
  )
)

; compare
(define (comp-string x y)
  (cond
    ((string=? x y) 0)
    ((string>? x y) 1)
    (else -1)
  )
)
(define (comp-symbol x y)
  (comp-string (symbol->string x) (symbol->string y))
)
(define (comp-number x y)
  (cond
    ((= x y) 0)
    ((> x y) 1)
    (else -1)
  )
)

; test-tree
; (define t1 (make-tree (cons 'a 101) '() '()))
; (insert-tree! (cons 'b 102) t1 comp-symbol)
; (display (lookup-tree 'a t1 comp-symbol))(newline)
; (display (lookup-tree 'b t1 comp-symbol))(newline)

(define (make-table comp)
  (define (assoc key records)
    (lookup-tree key records comp)
  )

  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (define (lookup-aux keys table)
        (let ((sub-table (assoc (car keys) (cdr table))))
          (if sub-table
            (if (null? (cdr keys))
              (cdr sub-table)
              (lookup-aux (cdr keys) sub-table)
            )
            #f
          )
        )
      )
      (lookup-aux key-list local-table)
    )

    (define (insert! key-list val)
      (define (make-entry keys)
        (if (null? (cdr keys))
          (cons (car keys) val)
          (cons (car keys) (make-tree (make-entry (cdr key-list)) '() '()))
        )
      )
      (define (insert-aux! keys table)
        (let ((sub-table (assoc (car keys) (cdr table))))
          (if sub-table
            ; exist
            (if (null? (cdr keys))
              (set-cdr! sub-table val)
              (insert-aux! (cdr keys) sub-table)
            )
            ; not exist
            (set-cdr! table (insert-tree! (make-entry keys) (cdr table) comp))
          )
        )
      )
      (insert-aux! key-list local-table)
      'ok
    )

    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown operation -- TABLE" m))
      )
    )
    dispatch
  )
)

; test
; 1-d
; (define table1 (make-table comp-number))
; (define get1 (table1 'lookup-proc))
; (define put1 (table1 'insert-proc!))
; (put1 '(100) 'a)
; (put1 '(200) 'b)
; (put1 '(50) 'c)
; (put1 '(75) 'd)
; (put1 '(60) 'e)
; (display (get1 '(50)))(newline) ;Value: c
; (display (get1 '(60)))(newline) ;Value: e
; (display (get1 '(75)))(newline) ;Value: d
; (display (get1 '(100)))(newline);Value: a
; (display (get1 '(200)))(newline) ;Value: b
; (display (get1 '(111)))(newline) ;Value: #f
; (put1 '(60) 'x)
; (display (get1 '(60)))(newline) ;Value: x
; (newline)

; ; 2-d
; (define table2 (make-table comp-number))
; (define get2 (table2 'lookup-proc))
; (define put2 (table2 'insert-proc!))
; (put2 '(50 500) "50,500")
; (put2 '(50 450) "50,450")
; (put2 '(60 300) "60,300")
; (put2 '(50 475) "50,475")
; (put2 '(50 480) "50,480")
; (put2 '(60 350) "60,350")
; (display (get2 '(50 450)))(newline) ;Value: "50,450"
; (display (get2 '(50 500)))(newline) ;Value: "50,500"
; (display (get2 '(60 350)))(newline) ;Value: "60,350"
