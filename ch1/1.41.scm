(define (double f)
  (lambda (x) (f (f x))) ; cannot write as `(f f x)`, which interprets as invoke f with f,x
)

(define (inc x) (+ x 1))

(display ((double inc) 1))(newline)

; (double double) => (f (f (f (f x)))) as do-four-times
; (double (do-four-times)) => do do-four-times four times as do-16-times
; (do-16-times inc) => do inc 16 times
; do-inc-16-times to 5
(display (((double (double double)) inc) 5))(newline)
; => 16
