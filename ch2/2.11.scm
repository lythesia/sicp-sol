(load "2.10.scm")

(define (both-pos? i) (> (lower-bound i) 0))
(define (both-neg? i) (< (upper-bound i) 0))
(define (mul-interval x y)
  (let* 
    (
     (x-lo (lower-bound x))
     (x-up (upper-bound x))
     (y-lo (lower-bound y))
     (y-up (upper-bound y))
    )
    (cond 
      ((> x-lo 0)
       (cond 
         ((> y-lo 0) (make-interval (* x-lo y-lo) (* x-up y-up)))
         ((< y-up 0) (make-interval (* x-up y-lo) (* x-lo y-up)))
         (else (make-interval (* x-up y-lo) (* x-up y-up)))
       )
      )
      ((< x-up 0)
       (cond 
         ((> y-lo 0) (make-interval (* x-lo y-up) (* x-up y-lo)))
         ((< y-up 0) (make-interval (* x-up y-up) (* x-lo y-lo)))
         (else (make-interval (* x-lo y-up) (* x-lo y-lo)))
       )
      )
      (else
        (cond 
          ((> y-lo 0) (make-interval (* x-lo y-up) (* x-up y-up)))
          ((< y-up 0) (make-interval (* x-up y-lo) (* x-lo y-lo)))
          (else (make-interval (min (* x-lo y-up) (* x-up y-lo)) (max (* x-lo y-lo) (* x-up y-up))))
        )
      )
    )
  )
)

; test
; (define i1 (make-interval 0 1))
; (define i2 (make-interval -1 0))
; (display i1)(newline)
; (display i2)(newline)
; (display (mul-interval i1 i2))(newline)
