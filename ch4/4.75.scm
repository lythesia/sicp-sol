(define (uniquely-asserted unique-assertion frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((result (qeval (unique-query unique-assertion) (singleton-stream frame))))
        (if (singleton-stream? result)
          result
          the-empty-stream
        )
      )
    )
    frame-stream
  )
)
(define (singleton-stream? s)
  (and
    (not (stream-null? s))
    (stream-null? (stream-cdr s))
  )
)
(put 'unique 'qeval uniquely-asserted)
