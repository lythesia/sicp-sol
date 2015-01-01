(load "util.scm")

(define (cycle? x)
  (define (iter p1 p2)
    (if (or (null? p1) (null? p2) (null? (cdr p2)))
      #f
      (let ((next-p1 (cdr p1)) (next-p2 (cddr p2)))
        (if (eq? next-p1 next-p2)
          #t
          (iter next-p1 next-p2)
        )
      )
    )
  )
  (if (pair? x)
    (iter x x)
    (error "pair expected.")
  )
)

; test
; (define l (list 1 2 3))
; (display (cycle? l))(newline)
; (display (cycle? (cons 0 (make-cycle l))))(newline)
