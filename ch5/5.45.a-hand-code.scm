(use-modules (ice-9 format))

(define test-values '(5 10 25 50 100 250 500 1000))

(load "5.05.regsim.scm")
(define (test-regism-with-hand-code)
  (define (make-factorial-machine)
    (make-machine
      '(continue n val)
      (list (list '= =) (list '- -) (list '* *))
      '(
        (assign continue (label fact-done))
        fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label base-case))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
        after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg n) (reg val))
        (goto (reg continue))
        base-case
        (assign val (const 1))
        (goto (reg continue))
        fact-done
      )
    )
  )
  (define (test n)
    (let ((machine (make-factorial-machine)))
      (set-register-contents! machine 'n n)
      ((machine 'stack) 'initialize)
      (start machine)
      ; (format #t "Factorial of ~a: ~a\n" n (get-register-contents machine 'val))
      ((machine 'stack) 'print-statistics)
    )
  )
  (for-each test test-values)
)
; (test-regism-with-hand-code)
; total-pushes: 8, maximum-depth: 8
; total-pushes: 18, maximum-depth: 18
; total-pushes: 48, maximum-depth: 48
; total-pushes: 98, maximum-depth: 98
; total-pushes: 198, maximum-depth: 198
; total-pushes: 498, maximum-depth: 498
; total-pushes: 998, maximum-depth: 998
; total-pushes: 1998, maximum-depth: 1998
