(load "util.scm")

(define (make-segment px py)
  (cons px py)
)

(define (start-segment l) (car l))
(define (end-segment l) (cdr l))

(define (midpoint-segment l)
  (let ((start (start-segment l)) (end (end-segment l)))
    (make-point (ave (x-point start) (x-point end))
                (ave (y-point start) (y-point end))
    )
  )
)

(define (make-point x y)
  (cons x y)
)

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")(newline)
)

; (define start (make-point 1 3))
; (define end (make-point 4 3))
; (define seg (make-segment start end))
; (define mid (midpoint-segment seg))
; (print-point mid)
