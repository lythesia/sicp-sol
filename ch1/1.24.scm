; fermat lemma
; n is a prime, a is any positive < n, then
; a^n = a mod n

(define (expmod base exp n)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) n)) n))
        (else (remainder (* base (expmod base (- exp 1) n)) n))
  )
)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a)
  )
  (try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)
  )
)

(define (next-odd n)
  (if (even? n) (+ n 1) (+ n 2))
)

(define (search-prime n)
  (if (fast-prime? n 10) n (search-prime (next-odd n)))
)

(define (time-search-prime n)
  (let ((start-time (real-time-clock)))
    (newline)(display (search-prime (next-odd n)))(newline)
    (- (real-time-clock) start-time)
  )
)

; (time-search-prime 100000000)
