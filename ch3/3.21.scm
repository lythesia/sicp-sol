(load "3.03.02.scm")

(define (print-queue q)
  (define (iter head)
    (cond
      ((null? head) (display "nil"))
      (else (display (car head)) (display " - ") (iter (cdr head)))
    )
  )
  (iter (front-ptr q))
)

; test
; (define q1 (make-queue))
; (insert-queue! q1 'a)
; (insert-queue! q1 'b)
; (print-queue q1)(newline)
; (delete-queue! q1)
; (delete-queue! q1)
; (print-queue q1)(newline)

; another sol for REPL
; (define (print-queue q) (car q))
