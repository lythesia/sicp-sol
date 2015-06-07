(define dont-run-lazy 1)
(include "4.02.02.delay.scm")

;; define lazy list op in global env
(actual-value
  ; exp
  '(begin
     (define (cons x y) (lambda (m) (m x y)))
     (define (car z) (z (lambda (p q) p)))
     (define (cdr z) (z (lambda (p q) q)))

     (define (list-ref items n)
       (if (= n 0)
         (car items)
         (list-ref (cdr items) (- n 1))
       )
     )
     (define (map proc items)
       (if (null? items)
         '()
         (cons (proc (car items)) (map proc (cdr items)))
       )
     )
     (define (scale-list items fac)
       (map (lambda (x) (* x fac)) items)
     )
     (define (add-lists l1 l2)
       (cond
         ((null? l1) l2)
         ((null? l2) l1)
         (else (cons (+ (car l1) (car l2)) (add-lists (cdr l1) (cdr l2))))
       )
     )

     (define ones (cons 1 ones))
     (define integers (cons 1 (add-lists ones integers)))

     (define (integral integrand initial dt)
       (define int
         (cons
           initial
           (add-lists (scale-list integrand dt) int)
         )
       )
       int
     )
     (define (solve f y0 dt)
       (define y (integral dy y0 dt))
       (define dy (map f y))
       y
     )
  )
  the-global-environment
)

(if (not (defined? 'dont-run-now))
  (driver-loop)
)
