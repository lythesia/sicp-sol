; 1-d
(define (make-table) (list '*table*))
(define (assoc key records)
  (cond
    ((null? records) #f)
    ((equal? key (caar records)) (car records))
    (else (assoc key (cdr records)))
  )
)
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record (cdr record) #f)
  )
)
(define (insert! key val table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record val)
      (set-cdr! table (cons (cons key val) (cdr table))) ; pre-append sub-table
    )
  )
)

(define memorize
  (lambda (f)
    ((lambda (table)
      (lambda (x) ; single x for f
        ((lambda (prev-computed-result)
           (or prev-computed-result
               ((lambda (result)
                  (insert! x result table)
                  result
                )
                (f x)
               )
           )
         )
         (lookup x table)
        )
      )
     )
     (make-table)
    )
  )
)
