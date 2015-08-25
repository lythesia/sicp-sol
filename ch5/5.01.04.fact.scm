(controller
  (assign continue (label fact-done))
fact-loop
  (test (op =) (reg n) (const 1))
  (branch (label base-case))

  (save continue) ; push
  (save n)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-fact))
  (goto (label fact-loop))

after-fact
  (restore n) ; pop
  (restore continue)
  (assign val (op *) (reg val) (reg n))
  (goto (reg continue))

base-case
  (assign val (const 1))
  (goto (reg continue))

fact-done
)
