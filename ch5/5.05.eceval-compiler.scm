;; should load eceval-support first

(define the-global-environment '())
(define (get-global-environment) the-global-environment)

;; eceval need to be defined
(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag #f)
  (start eceval)
)

(define (compile-and-go exp)
  (let* ((text (statements (compile exp 'val 'return))) (instructions (assemble text eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag #t)
    (start eceval)
  )
)

(define eceval-operations
  (list
    ; env
    (list 'get-global-environment get-global-environment)
    ; repl
    (list 'prompt-for-input prompt-for-input)
    (list 'read read)
    (list 'display display)
    (list 'announce-output announce-output)
    (list 'user-print user-print)
    (list 'eof-object? eof-object?)
    (list 'exit exit)
    ; parse
    (list 'self-evaluating? self-evaluating?)
    (list 'variable? variable?)
    (list 'quoted? quoted?)
    (list 'assignment? assignment?)
    (list 'definition? definition?)
    (list 'if? if?)
    (list 'lambda? lambda?)
    (list 'begin? begin?)
    (list 'application? application?)
    (list 'cond? cond?)
    (list 'cond->if cond->if)
    (list 'cond-clauses cond-clauses)
    (list 'cond-first-clause cond-first-clause)
    (list 'cond-rest-clauses cond-rest-clauses)
    (list 'cond-first-clause-predicate cond-first-clause-predicate)
    (list 'cond-first-clause-actions cond-first-clause-actions)
    (list 'cond-actions cond-actions)
    (list 'cond-predicate cond-predicate)
    (list 'cond-else-clause? cond-else-clause?)
    (list 'cond-no-clauses? cond-no-clauses?)
    (list 'let? let?)
    (list 'let->combination let->combination)
    (list 'lookup-variable-value lookup-variable-value)
    (list 'text-of-quotation text-of-quotation)
    (list 'lambda-parameters lambda-parameters)
    (list 'lambda-body lambda-body)
    (list 'make-procedure make-procedure)
    (list 'operands operands)
    (list 'operator operator)
    (list 'empty-arglist empty-arglist)
    (list 'no-operands? no-operands?)
    (list 'first-operand first-operand)
    (list 'last-operand? last-operand?)
    (list 'rest-operands rest-operands)
    (list 'adjoin-arg adjoin-arg)
    (list 'primitive-procedure? primitive-procedure?)
    (list 'compound-procedure? compound-procedure?)
    (list 'apply-primitive-procedure apply-primitive-procedure)
    (list 'procedure-parameters procedure-parameters)
    (list 'procedure-environment procedure-environment)
    (list 'procedure-body procedure-body)
    (list 'extend-environment extend-environment)
    (list 'begin-actions begin-actions)
    (list 'first-exp first-exp)
    (list 'last-exp? last-exp?)
    (list 'rest-exps rest-exps)
    (list 'if-predicate if-predicate)
    (list 'true? true?)
    (list 'false? false?)
    (list 'if-alternative if-alternative)
    (list 'if-consequent if-consequent)
    (list 'assignment-variable assignment-variable)
    (list 'assignment-value assignment-value)
    (list 'set-variable-value! set-variable-value!)
    (list 'definition-variable definition-variable)
    (list 'definition-value definition-value)
    (list 'define-variable! define-variable!)
    (list 'make-compiled-procedure make-compiled-procedure)
    (list 'compiled-procedure? compiled-procedure?)
    (list 'compiled-procedure-entry compiled-procedure-entry)
    (list 'compiled-procedure-env compiled-procedure-env)
    ; base
    (list 'cons cons)
    (list 'list list)
    ; open-code
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '= =)
  )
)

(define eceval-text `(
  (branch (label external-entry))
repl
  (perform (op initialize-stack))
  (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))

print-result
  ,(if (defined? 'profile-stack)
     '(perform (op print-stack-statistics)))
  (perform (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label repl))

external-entry
  (perform (op initialize-stack))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (reg val))

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
  (test (op cond?) (reg exp))
  (branch (label ev-cond))
  (test (op let?) (reg exp))
  (branch (label ev-let))
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

ev-cond
  (assign exp (op cond->if) (reg exp))
  (goto (label ev-if))

ev-let
  (assign exp (op let->combination) (reg exp))
  (perform (op user-print) (reg exp))
  (goto (label ev-application))

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
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-apply))
  (goto (label unknown-procedure-type))
compiled-apply
  (restore continue)
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
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
  (perform (op display) (const "Bye ~\n"))
  (perform (op exit))
))

(define eceval (make-machine '(exp env val proc argl continue unev) eceval-operations eceval-text))
(if (defined? 'open-code)
  (begin
    ((eceval 'allocate-register) 'arg1)
    ((eceval 'allocate-register) 'arg2)
  )
)
