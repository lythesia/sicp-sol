;; in make-new-machine
(let (
; ..
  (instruction-number 0))
; ..
  (define (execute)
    (let ((insts (get-contents pc)))
      (if (null? insts) 'done
        (begin
          (instruction-execution-proc (car insts))
          (set! instruction-number (+ instruction-number 1))
          (execute)
        ))
    )
  )
  (define (print-instruction-number)
    (display "current instruction number: ")(display instruction-number)(newline)
    (set! instruction-number 0)
  )
)
