(load "util.scm") ; for average
(load "3.05.01.stream.scm")
(load "3.50.scm")
(load "3.05.02.is.scm")
(load "3.55.scm") ; partial-sums
(load "3.05.03.acc.scm")

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
    (stream-map - (ln2-summands (1+ n)))
  )
)

(define ln2-stream (partial-sums (ln2-summands 1)))
(define ln2-stream-acc-1 (euler-transform ln2-stream))
(define ln2-stream-acc-2 (euler-transform ln2-stream-acc-1))

; test
; (display-stream-to ln2-stream 10)(newline)
; (display-stream-to ln2-stream-acc-1 5)(newline)
; (display-stream-to ln2-stream-acc-2 2)(newline)
