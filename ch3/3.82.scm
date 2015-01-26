(load "util.scm")
(load "3.05.01.stream.scm")
(load "3.50.scm")
(load "3.05.02.is.scm")
(load "3.81.scm")

(define rand-max 2147483648.0)
(define random-init 4)
(define rand-numbers (cons-stream random-init (stream-map rand-update rand-numbers)))
(define float-rand-numbers (stream-map (lambda (x) (/ x rand-max)) rand-numbers)) ; [0, 1.0]

(define (map-succ-pairs f s)
  (cons-stream
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-succ-pairs f (stream-cdr s))
  )
)

(define (monte-carlo exp-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo (stream-cdr exp-stream) passed failed)
    )
  )
  (if (stream-car exp-stream)
    (next (1+ passed) failed)
    (next passed (1+ failed))
  )
)

(define radius 0.5)
(define (in-circle? x y)
  (< (+ (square (- x radius)) (square (- y radius))) (square radius))
)

(define (estimate-integral pred? x1 y1 x2 y2)
  (monte-carlo
    (map-succ-pairs
      (lambda (x y)
        ; coor transform to [(x1,y1), (x2,y2)] space
        (let ((xx (+ x1 (* x (- x2 x1)))) (yy (+ y1 (* y (- y2 y1)))))
          (pred? xx yy)
        )
      )
      float-rand-numbers
    )
    0
    0
  )
)

; I think follow happen to be right since $ 1 /  radius^2 == 4 $ 
; (define pi
;   (stream-map (lambda (x) (/ x (square radius))) (estimate-integral in-circle? 0.0 0.0 1.0 1.0))
; )

; (define pi (scale-stream (estimate-integral in-circle? 0.0 0.0 1.0 1.0) 4.0))
; (display (stream-ref pi 1000))(newline) ; => 3.1528471528471527
