; Miller-Rabin test

(define (square n) (* n n))

(define (trivial-square-test a n)
  (if (and (not (= a 1))
           (not (= a (- n 1)))
           (= (remainder (square a) n) 1))
    0 ; fail
    a ; success, return the remainder
  )
)

(define (expmod base exp n)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square(trivial-square-test (expmod (square base) (/ exp 2) n) n)) n)) ; check remainder q non-trivial, then as usual q^2 | a
        (else (remainder (* base (expmod base (- exp 1) n)) n))
  )
)

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1)
  )
  (try-it (+ 1 (random(- n 1))))
)

(define (do-miller-rabin-test n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (do-miller-rabin-test n (- times 1)))
        (else false)
  )
)

(display (do-miller-rabin-test 561 10))(newline)
