; generic map
(define (stream-map proc . argstreams)
  (if (empty-stream? (car argstreams))
    the-empty-stream
    ( cons-stream
      (apply proc (map stream-car argstreams))  ; proc needs all 1st arg of each stream
      (apply stream-map (cons proc (map stream-cdr argstreams)))  ; apply on rest
    )
  )
)
