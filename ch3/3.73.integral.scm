(load "3.05.01.stream.scm")
(load "3.50.scm") ; generic map
(load "3.05.02.is.scm") ; add-streams

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value (add-streams (scale-stream integrand dt) int)) ; give self a name
  )
  int
)
