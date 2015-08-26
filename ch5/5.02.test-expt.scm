(use-modules (ice-9 format))
(include "5.02.all.scm")

(define expt-machine
  (make-machine
   '(b n r continue)
   (list (list '= =) (list '* *) (list '- -))
   '(
     (assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label expt-base))
     (save continue)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore continue)
     (assign r (op *) (reg r) (reg b))
     (goto (reg continue))
     expt-base
     (assign r (const 1))
     (goto (reg continue))
     expt-done
    )
  )
)

(display "init:\n")
(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 10)
(format #t "reg[b] = ~d\n" (get-register-contents expt-machine 'b))
(format #t "reg[n] = ~d\n" (get-register-contents expt-machine 'n))
(start expt-machine)

(display "\nafter:\n")
(format #t "reg[r] = ~d\n" (get-register-contents expt-machine 'r))
