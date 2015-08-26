;; in make-new-machine
(let (
; ..
  (trace-flag #f))
; ..
  (define (execute)
    (let ((insts (get-contents pc)))
      (if (null? insts) 'done
        (begin
          (if trace-flag (begin (display (instruction-text (car insts))) (newline)))
          (instruction-execution-proc (car insts))
          (execute)
        )
      )
    )
  )
  (define (trace-on)
    (display "** trace on **\n")
    (set! trace-flag #t)
  )
  (define (trace-off)
    (display "** trace off **\n")
    (set! trace-flag #f)
  )
)
