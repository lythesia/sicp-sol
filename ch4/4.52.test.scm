(define (an-element-of lst)
  (require (not (null? lst)))
  (amb (car lst) (an-element-of (cdr lst)))
)

(define (require p)
  (if (not p) (amb))
)

(define (even? x)
  (= (remainder x 2) 0)
)

(if-fail
  (let
    ((x (an-element-of '(1 3 5))))
    (require (even? x))
    x
  )
  'all-odd
)

(if-fail
  (let
    ((x (an-element-of '(1 3 5 8))))
    (require (even? x))
    x
  )
  'all-odd
)

quit
