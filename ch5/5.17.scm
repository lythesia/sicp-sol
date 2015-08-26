;; modify implementation in 5.02.02.as.scm

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels
      (cdr text)
      (lambda (insts labels)
        (let ((next-inst (car text)))
          (if (symbol? next-inst)
            (let
              ((insts
                 (if (null? insts) '()
                   (cons (make-instruction-with-label (instruction-text (car insts)) next-inst) (cdr insts))
                 )))
              (receive insts (cons (make-label-entry next-inst insts) labels))
            )
            (receive (cons (make-instruction next-inst) insts) labels)
          )
        )
      )
    )
  )
)
; inst: (code-text exec-proc label), this label is only a symbol
(define (make-instruction text) (list text '() '()))
(define (make-instruction-with-label text label) (list text '() label))
(define instruction-text car)
(define instruction-execution-proc cadr)
(define instruction-label caddr)
(define (set-instruction-execution-proc! inst proc) (set-car! (cdr inst) proc))


;; modify in make-new-machine
(define (execute)
  (let ((insts (get-contents pc)))
    (if (null? insts) 'done
      (begin
        (if trace-flag
          (begin
            (if (not (null? (instruction-label (car (insts)))))
              (begin
                (display (instruction-label (car insts)))
                (newline)
              )
            )
            (display (instruction-text (car insts)))(newline)
          )
        )
        (instruction-execution-proc (car insts))
        (execute)
      )
    )
  )
)
