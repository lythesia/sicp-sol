; a)
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s))
)
(define (simple-flatten stream)
  (stream-map
    stream-car
    (stream-filter (lambda (frame-stream) (not (stream-null? frame-stream))) stream)
  )
)

; b)
; no.
