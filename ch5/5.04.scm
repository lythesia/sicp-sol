; a) recursive
(controller
  (assign continue (label expt-done))

expt-loop
  (test (op =) (reg n) (const 0))
  (branch (label base-case))
  (save continue)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-expt))
  (goto (label fact-loop))

after-expt
  (restore continue)
  (assign val (op *) (reg b) (reg val))
  (goto (reg continue))

base-case
  (assign val (const 1))
  (goto (reg continue))
expt-done
)

; b) iterative
(controller
  ; init
  (assign val (const 1))
  (assign counter (reg n))

test-counter
  (test (op =) (reg counter) (const 0))
  (branch (label expt-done))
  (assign val (op *) (reg b) (reg val))
  (assign counter (op -) (reg counter) (const 1))
  (goto (label test-counter))
expt-done
)
