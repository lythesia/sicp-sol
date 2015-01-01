(load "util.scm") ; for make-cycle

(define (cycle? x)
  (define (iter rest memo-list)
    (cond
      ((null? rest) #f)
      ((memq rest memo-list) #t)
      (else (iter (cdr rest) (cons rest memo-list)))
    )
  )
  (if (pair? x)
    (iter x '())
    (error "pair expected.")
  )
)

; test
; (define l (list 1 2 3))
; (display (cycle? (make-cycle l)))(newline)
; (define l2 (list 1 2 3))
; (display (cycle? (cons 0 (make-cycle l2))))(newline)
; (display (cycle? 'a))(newline)
; (display (cycle? (list 0 1 (make-cycle '(3 4 5)) 6)))(newline) ; #f
