(load "util.scm")

; point (<x> . <y>)
;       ^
;       |
; ------+------->
;       |
;       |

(define (make-point x y) (cons x y))
(define (xcoord p) (car p))
(define (ycoord p) (cdr p))

(define (estimate-integral pred? left-top right-bot trials)
  (* 4 (monte-carlo trials (lambda () (pred? left-top right-bot))))
)

(define (monte-carlo trials experiment)
  (define (iter hit rest)
    (if (zero? rest) (exact->inexact (/ hit trials))
      (if (experiment)
        (iter (1+ hit) (1- rest))
        (iter hit (1- rest))
      )
    )
  )
  (iter 0 trials)
)

(define (random-in-range low high)
  (+ low (random (exact->inexact (- high low)))) ; `random` in scheme outputs int/float depends on input
)

(define (picked-in-range? lt rb)
  (let*
    (
      (lt-x (xcoord lt)) (lt-y (ycoord lt))
      (rb-x (xcoord rb)) (rb-y (ycoord rb))
      (cx (/ (exact->inexact (+ rb-x lt-x)) 2)) (cy (/ (exact->inexact (+ lt-y rb-y)) 2))
      (px (random-in-range lt-x rb-x)) (py (random-in-range rb-y lt-y))
      (r (/ (exact->inexact (- rb-x lt-x)) 2))
    )
    (<= (+ (square (- cx px)) (square (- cy py))) (square r))
  )
)

; test
; (define lt (make-point -1.0 1.0))
; (define rb (make-point 1.0 -1.0))
; (define (est-pi trials) (estimate-integral picked-in-range? lt rb trials))
; (display (est-pi 1000))(newline)
; (display (est-pi 10000))(newline)
; (display (est-pi 100000))(newline)
