(load "util.scm")
; fixed-point f init eps
(load "1.36.scm")
; repeated f n
(load "1.43.scm")

(define (average-damp f) (lambda (x) (/ (+ x (f x)) 2)))
(define (average-damp-n-times f n)
  ((repeated-fast average-damp n) f)
)

; b^n
(define (expt b n)
  (if (= n 0) 1
    ((repeated-fast (lambda (x) (* b x)) n) 1)
  )
)

; (display (expt 2 5))(newline)
; (display ((average-damp-n-times square 10) 10.0))(newline)

(define (damped-nth-root n damp-times)
  (lambda (x)
    (fixed-point 
      (average-damp-n-times (lambda (y) (/ x (expt y (- n 1)))) damp-times) 1.0 eps)
  )
)

(define sqrt (damped-nth-root 2 1))
(define cube-root (damped-nth-root 3 1))
(define quad-root (damped-nth-root 4 2))

; (display (sqrt (* 3 3)))(newline)
; (display (cube-root (* 3 3 3)))(newline)
; (display (quad-root (* 3 3 3 3)))(newline)

; some conclusion: n-th root need log(n) times of damp

(define (lg2 n)
  (cond ((> (/ n 2) 1) (+ 1 (lg2 (/ n 2))))
        ((< (/ n 2) 1) 0)
        (else 1)
  )
)

(define (n-th-root n)
  (damped-nth-root n (lg2 n))
)

(define sqrt (n-th-root 2))
(define cube-root (n-th-root 3))
(define quad-root (n-th-root 4))
(define hyaku-root (n-th-root 100))

(display (sqrt (* 3 3)))(newline)
(display (cube-root (* 3 3 3)))(newline)
(display (quad-root (* 3 3 3 3)))(newline)
(display (hyaku-root 100))(newline)
