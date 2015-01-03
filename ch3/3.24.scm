(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    ; fetch key-related record in records or false
    (define (assoc key records)
      (cond
        ((null? records) #f)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records)))
      )
    )
    ; fetch the content of record
    (define (lookup key-1 key-2)
      (let ((sub-table (assoc key-1 (cdr local-table))))
        (if sub-table
          (let ((record (assoc key-2 (cdr sub-table))))
            (if record (cdr record) #f)
          )
          #f
        )
      )
    )

    (define (insert! key-1 key-2 val)
      (let ((sub-table (assoc key-1 (cdr local-table))))
        (if sub-table
          (let ((record (assoc key-2 (cdr sub-table))))
            (if record
              (set-cdr! record val) ; modify
              (set-cdr! sub-table (cons (cons key-2 val) (cdr sub-table))) ; pre-append sub-table
            )
          )
          (set-cdr! local-table (cons (list key-1 (cons key-2 val)) (cdr local-table))) ; pre-append local-table
        )
      )
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
