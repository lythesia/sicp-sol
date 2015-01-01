(define (count-pairs l)
  (let ((memo-list '()))
    (define (count z)
      (cond
        ((not (pair? z)) 0)
        ((memq z memo-list) 0)
        (else
          (if (null? memo-list)
            (set! memo-list (list z))                 ; add self pointer, so cycle will be detected via memq
            (set-cdr! (last-pair memo-list) (list z)) ; append last as pointer
          )
          ; (display memo-list)(newline)
          (+ (count (car z)) (count (cdr z)) 1)
        )
      )
    )
    (count l)
  )
)

; test
; cycle
; (load "util.scm")
; (define cyc (make-cycle (list 1 2 3)))
; (display (count-pairs cyc))(newline)
; (display (count-pairs (cons 0 cyc)))(newline)
