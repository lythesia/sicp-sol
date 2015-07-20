(define (conjoin conjuncts frame-stream)
  (define (recur first-frame-stream rest-frame-stream) ; rest is stream of streams
    (if (stream-null? rest-frame-stream)
      first-frame-stream
      (recur (join first-frame-stream (stream-car rest-frame-stream)) (stream-cdr rest-frame-stream))
    )
  )
  (if (empty-conjunction? conjuncts)
    frame-stream
    (let
      ((first (qeval (first-conjunct conjuncts) frame-stream))
       (rest (qeval (rest-conjuncts conjuncts) frame-stream)))
      (recur first rest)
    )
  )
)

; how to implement join?
; merge two frames, for each binding in f1, try to extend it into f2
(define (merge-frames f1 f2)
  (if (null? f1)
    f2
    (let ((first-binding (car f1)))
      (let ((maybe-extended-frame (extend-if-possible (binding-variable first-binding) (binding-value first-binding) f2)))
        (if (eq? maybe-extended-frame 'failed)
          'failed
          (merge-frames (cdr f1) maybe-extended-frame)
        )
      )
    )
  )
)
; merge two streams
(define (join s1 s2)
  (stream-flatmap
    (lambda (f1)
      (stream-filter
        (lambda (f) (not (eq? f 'failed)))
        (stream-map (lambda (f2) (merge-frames f1 f2)) s2)
      )
    )
    s1
  )
)
