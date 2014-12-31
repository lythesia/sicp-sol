(define (make-accumulator sum)
  (lambda (x)
    (begin (set! sum (+ sum x)) sum)
  )
)

; test
; (define acc (make-accumulator 5))
; (display (acc 10))(newline)
; (display (acc 10))(newline)
; (define another-acc (make-accumulator 0))
; (display (another-acc 2))(newline)
; (display (another-acc 4))(newline)
