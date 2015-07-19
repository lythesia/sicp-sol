(use-modules (srfi srfi-41))

(define the-empty-stream (stream))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
    (force delayed-s2)
    (stream-cons
      (stream-car s1)
      (stream-append-delayed (stream-cdr s1) delayed-s2)
    )
  )
)

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
    (force delayed-s2)
    (stream-cons
      (stream-car s1)
      (interleave-delayed (force delayed-s2) (delay (stream-cdr s1)))
    )
  )
)

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s))
)
(define (flatten-stream s)
  (if (stream-null? s)
    the-empty-stream
    (interleave-delayed
      (stream-car s) ; it's also a stream
      (delay (flatten-stream (stream-cdr s)))
    )
  )
)
(define (singleton-stream x) (stream-cons x the-empty-stream))

;; util
(define (display-stream s)
  (define (disp-line x)
    (newline)
    (display x)
  )
  (stream-for-each disp-line s)
)
