; pre
; in guile:
; (define-syntax delay 
;     (syntax-rules ()
;         [(delay expr) (cons 'promise (lambda () expr) )]))

; (define (force promise)
;     (let*
;         ((func (cdr promise))
;          (value (func)))
;         (set-cdr! promise (lambda () value))
;         value))

; (define-syntax delay (syntax-rules () [(delay expr) (lambda () expr)]))
; memorized
(define-syntax delay (syntax-rules () [(delay expr) (memo-proc (lambda () expr))]))
(define (force f) (f))

; def
; (define (cons-stream a b) (cons a (delay b))) ; this not work
(define-syntax cons-stream (syntax-rules () [(cons-stream a b) (cons a (delay b))]))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define the-empty-stream '())
(define empty-stream? null?)

; util
; access s[n]
(define (stream-ref s n)
  (if (zero? n) (stream-car s) (stream-ref (stream-cdr s) (1- n)))
)

; 1-to-1 map
(define (stream-map proc s)
  (if (empty-stream? s) the-empty-stream
    (cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr s)))
  )
)

; for-each
(define (stream-for-each proc s)
  (if (empty-stream? s) 'done
    (begin (proc (stream-car s)) (stream-for-each proc (stream-cdr s)))
  )
)
(define (stream-iter-to proc s to)
  (if (or (empty-stream? s) (zero? to)) 'done
    (begin (proc (stream-car s)) (stream-iter-to proc (stream-cdr s) (1- to)))
  )
)

; display
(define (display-stream s)
  (stream-for-each display-line s)
)
(define (display-stream-to s to)
  (stream-iter-to display-line s to)
)
(define (display-line x) (newline)(display x))

; gen interval
(define (stream-enum-interval l h)
  (if (> l h) the-empty-stream
    (cons-stream l (stream-enum-interval (1+ l) h))
  )
)

; filter
(define (stream-filter pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (stream-car s)) (cons-stream (stream-car s) (stream-filter pred (stream-cdr s))))
    (else (stream-filter pred (stream-cdr s)))
  )
)

; memo (non-param proc)
(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
        (begin
          (set! result (proc))
          (set! already-run? #t)
          result
        )
        result
      )
    )
  )
)
