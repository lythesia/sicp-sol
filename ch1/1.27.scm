(load "1.24.scm")

(define (carmichael-test n)
  (test-iter n 1)
)

(define (test-iter n a)
  (cond ((= a n) true)
        ((= (expmod a n n) a) (test-iter n (+ a 1)))
        (else false)
  )
)

(display (carmichael-test 561))(newline)
(display (carmichael-test 1105))(newline)
(display (carmichael-test 1729))(newline)
(display (carmichael-test 2465))(newline)
(display (carmichael-test 2821))(newline)
(display (carmichael-test 6601))(newline)
