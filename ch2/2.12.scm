(load "2.11.scm")

(define (make-center-percent c p)
  (let ((delta (* 0.01 p c)))
    (make-interval (- c delta) (+ c delta))
  )
)

(define (center cp)
  (/ (+ (lower-bound cp) (upper-bound cp)) 2)
)

(define (percent cp)
  (/ (- (upper-bound cp) (lower-bound cp)) (+ (lower-bound cp) (upper-bound cp)) 0.01)
)

; (define cp (make-center-percent 15 10))
; (display cp)(newline)
; (display (center cp))(newline)
; (display (percent cp))(newline)
