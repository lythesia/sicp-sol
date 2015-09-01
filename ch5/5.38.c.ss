(assign val (op make-compiled-procedure) (label entry1) (reg env))
(goto (label after-lambda2))
entry1
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (n)) (reg argl) (reg env))
(assign arg1 (op lookup-variable-value) (const n) (reg env))
(assign arg2 (const 1))
(assign val (op =) (reg arg1) (reg arg2))
(test (op false?) (reg val))
(branch (label false-branch4))
true-branch3
(assign val (const 1))
(goto (reg continue))
false-branch4
(save continue)
(save env)    ; 1. here we correctly save env
(assign proc (op lookup-variable-value) (const factorial) (reg env))
(assign arg1 (op lookup-variable-value) (const n) (reg env))
(assign arg2 (const 1))
(assign val (op -) (reg arg1) (reg arg2))
(assign argl (op list) (reg val))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch6))
compiled-branch7
(assign continue (label proc-return9))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
proc-return9
(assign arg1 (reg val))
(goto (label after-call8))
primitive-branch6
(assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
after-call8
(restore env) ; 2. for here we can fetch the right `n` when comeback
(assign arg2 (op lookup-variable-value) (const n) (reg env))
(assign val (op *) (reg arg1) (reg arg2))
(restore continue)
(goto (reg continue))
after-if5
after-lambda2
(perform (op define-variable!) (const factorial) (reg val) (reg env))
(assign val (const ok))
