(assign proc (op make-compiled-procedure) (label entry1) (reg env))
(goto (label after-lambda2))
entry1
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
(assign val (op make-compiled-procedure) (label entry3) (reg env))
(goto (reg continue))
entry3
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (a b c d e)) (reg argl) (reg env))
(assign proc (op make-compiled-procedure) (label entry5) (reg env))
(goto (label after-lambda6))
entry5
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (y z)) (reg argl) (reg env))
(assign proc (op lookup-variable-value) (const *) (reg env))
(assign val (op lexical-address-lookup) (const (0 1)) (reg env))  ; z of (* x y z)
(assign argl (op list) (reg val))
(assign val (op lexical-address-lookup) (const (0 0)) (reg env))  ; y of (* x y z)
(assign argl (op cons) (reg val) (reg argl))
(assign val (op lexical-address-lookup) (const (2 0)) (reg env))  ; x of (* x y z)
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch7))
compiled-branch8
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch7
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call9
after-lambda6
(save continue)
(save proc)
(save env)
(assign proc (op lookup-variable-value) (const +) (reg env))
(assign val (op lexical-address-lookup) (const (1 0)) (reg env))  ; x of (+ c d x)
(assign argl (op list) (reg val))
(assign val (op lexical-address-lookup) (const (0 3)) (reg env))  ; d
(assign argl (op cons) (reg val) (reg argl))
(assign val (op lexical-address-lookup) (const (0 2)) (reg env))  ; c
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch13))
compiled-branch14
(assign continue (label after-call15))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch13
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call15
(assign argl (op list) (reg val))
(restore env)
(save argl)
(assign proc (op lookup-variable-value) (const *) (reg env))
(assign val (op lexical-address-lookup) (const (1 0)) (reg env))  ; x of (* a b x)
(assign argl (op list) (reg val))
(assign val (op lexical-address-lookup) (const (0 1)) (reg env))  ; b
(assign argl (op cons) (reg val) (reg argl))
(assign val (op lexical-address-lookup) (const (0 0)) (reg env))  ; a
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch10))
compiled-branch11
(assign continue (label after-call12))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch10
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call12
(restore argl)
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(restore continue)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch16))
compiled-branch17
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch16
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call18
after-lambda4
after-lambda2
(assign val (const 4))
(assign argl (op list) (reg val))
(assign val (const 3))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch19))
compiled-branch20
(assign continue (label proc-return22))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
proc-return22
(assign proc (reg val))
(goto (label after-call21))
primitive-branch19
(assign proc (op apply-primitive-procedure) (reg proc) (reg argl))
after-call21
(assign val (const 5))
(assign argl (op list) (reg val))
(assign val (const 4))
(assign argl (op cons) (reg val) (reg argl))
(assign val (const 3))
(assign argl (op cons) (reg val) (reg argl))
(assign val (const 2))
(assign argl (op cons) (reg val) (reg argl))
(assign val (const 1))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch23))
compiled-branch24
(assign continue (label after-call25))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch23
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call25
