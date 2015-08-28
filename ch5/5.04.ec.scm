(define scm-apply apply)  ; protect apply, since it has be override in ch4
(include "5.04.ec-operations.scm")

(define apply scm-apply)
(include "5.02.all.scm")

;; include this file **after** define/set `extra-dispatches, extra-ev-labels`

(define eceval-text `(
repl
  (perform (op initialize-stack))
  (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))

print-result
  (perform (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label repl))

unknown-expression-type
  (assign val (const unknown-expression-type))
  (goto (label error))
unknown-procedure-type
  (assign val (const unknown-procedure-type))
  (goto (label error))
error
  (perform (op user-print) (reg val))
  (perform (op user-print) (const ": "))
  (perform (op user-print) (reg exp))
  (perform (op user-print) (const #\nl))
  (goto (label repl))

eval-dispatch
  (test (op eof-object?) (reg exp))
  (branch (label end-of-file))
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  ; {{ entry for extra dispatch
  ,@(if (not (defined? 'extra-dispatches)) '() extra-dispatches)
  ; }}
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))
ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure) (reg unev) (reg exp) (reg env)) ; val now is proc
  (goto (reg continue))

  ; {{ entry for extra dispatch codes
  ,@(if (not (defined? 'extra-ev-labels)) '() extra-ev-labels)
  ; }}

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))  ; first eval operator(get real operator/proc)
  (assign continue (label ev-appl-did-operator))  ; for sake of returning to the point after eval operator
  (goto (label eval-dispatch))
ev-appl-did-operator
  ; restores pairs with ev-application
  (restore unev)
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val)) ; val from op make-procedure
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch)) ; if no args
  (save proc)             ; else save proc first, we try compose args
ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev)) ; consume args in unev one-by-one from left to right(since first-operand as car)
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg)) ; if it's last arg
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))  ; else we accumulate it after(since continue been set, and it will be protected if it'll be modified like in ev-application) eval it
  (goto (label eval-dispatch))
ev-appl-accumulate-arg
  ; restores pairs with ev-appl-operand-loop
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))  ; since evaled arg kept in val
  (assign unev (op rest-operands) (reg unev))         ; update unev(reduce one arg headly)
  (goto (label ev-appl-operand-loop))
ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))

apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))
primitive-apply
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)
  (goto (reg continue))
compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))
  ; before this should save continue (ref 5.24.scm, we save continue in ev-cond)
ev-sequence
  ; sequences stored in unev
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-last-exp
  (restore continue)  ; pair with ev-begin's save
  (goto (label eval-dispatch)) ; at this point, exp holds last exp of seq, and it will be evaled, result in val

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))  ; eval if-predicate
  (goto (label eval-dispatch))
ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))  ; eval assignment-value
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))  ; eval assignment-value
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

end-of-file
  (perform (op exit))
))

(define eceval
  (make-machine
    '(exp env val continue proc argl unev)  ; reg
    eceval-operations ; ops
    eceval-text ; code
  )
)

(if (not (defined? 'dont-run-ec))
  (start eceval)
)
