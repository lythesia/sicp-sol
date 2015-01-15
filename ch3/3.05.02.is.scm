(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (1+ n)))
)
; derived
(define integers (integers-starting-from 1))

; add zip-two-stream
(define (add-streams s1 s2)
  (stream-map + s1 s2)
)

; scale
(define (scale-stream s f) (stream-map (lambda (x) (* x f)) s))

; base
(define ones (cons-stream 1 ones))

(define fibs
  (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs)))
)

; mutula recursive def
(define primes
  (cons-stream 2 (stream-filter prime? (integers-starting-from 3)))
)
(define (prime? n)
  (define (iter ps)
    (let ((x (stream-car ps)))
      (cond
        ((> (expt x 2) n) #t)
        ((divisible? n x) #f)
        (else (iter (stream-cdr ps)))
      )
    )
  )
  (iter primes) ; with assumption: $p_{n+1} \le p_{n}^{2}$
)
(define (divisible? x y) (zero? (remainder x y)))
