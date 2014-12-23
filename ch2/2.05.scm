(load "../ch1/1.16.scm")

(define (cons a b)
  (* (fast-expt 2 a) (fast-expt 3 b))
)

(define (fin-lg n b)
  (define (iter curr c)
    (if (> (remainder curr b) 0) c
      (iter (/ curr b) (+ c 1))
    )
  )
  (iter n 0)
)

; fast-power like
(define (fin-lg x b)
  (define (iter n d c)
    (cond ((> (remainder n d) 0) 0)
          ((> (remainder n (* d d)) 0) (+ (iter (/ n d) b 1) c))
          (else (iter n (* d d) (+ c c)))
    )
  )
  (iter x b 1)
)

(define (car z)
  (fin-lg z 2)
)
(define (cdr z)
  (fin-lg z 3)
)

(define c (cons 1 1))
(display (car c))(newline)
(display (cdr c))(newline)
