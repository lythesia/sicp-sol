(define dont-run 1)
(include "4.01.no-aly.scm")

(define (amb? exp) (tagged-list? exp 'amb)) ; where amb is a special application?
(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail)
)
;; (analyze exp) gives a proc, this proc exec wiht env will compute value, so (analyze exp) env ==> value
;; then this value pass to succeed, if value eval(at this point) is succeed, then call succeed, which
;; return value, or call fail
;; succeed is `(lambda (value fail) value)`
;; fail is `(lambda () 'failed)`

(define (analyze exp)
  (cond
    ((self-evaluating? exp) (analyze-self-evaluating exp))

    ((variable? exp) (analyze-variable exp))

    ((quoted? exp) (analyze-quoted exp))

    ((assignment? exp) (analyze-assignment exp))
    ((definition? exp) (analyze-definition exp))

    ((if? exp) (analyze-if exp))

    ((lambda? exp) (analyze-lambda exp))

    ((begin? exp) (analyze-sequence (begin-actions exp)))
    
    ((cond? exp) (analyze (cond->if exp)))

    ((let? exp) (analyze (let->combination exp)))

    ((amb? exp) (analyze-amb exp))
    
    ((application? exp) (analyze-application exp))

    (else (error "Unknown expression type -- ANALYZE" exp))
  )
)

;; simple exp
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail) (succeed exp fail))
)

(define (analyze-variable exp)
  (lambda (env succeed fail) (succeed (lookup-variable-value exp env) fail))
)

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail)
      )
  )
)

(define (analyze-lambda exp)
  (let ((params (lambda-parameters exp)) (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure params bproc env) fail) ; `make-procedure` from 4.01.03.env.scm
    )
  )
)

;; cond & seq
(define (analyze-if exp)
  (let
    ((p-proc (analyze (if-predicate exp)))
     (c-proc (analyze (if-consequent exp)))
     (a-proc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (p-proc
        env
        (lambda (pred-value fail2) ;; <- this is a *succeed*, this pred-value will be fed by other real "eval"
          (if (true? pred-value) (c-proc env succeed fail2) (a-proc env succeed fail2))
        )
        fail
      )
    )
  )
)

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail)
      (proc1
        env
        (lambda (proc1-value fail2)
          (proc2 env succeed fail2)
        )
        fail
      )
    )
  )
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs)) (cdr rest-procs))
    )
  )
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE")
    )
    (loop (car procs) (cdr procs))
  )
)

;; def & assign
(define (analyze-definition exp)
  (let ((var (definition-variable exp)) (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      ;; need to eval (vproc env) first here
      (vproc
        env
        (lambda (val fail2)
          (define-variable! var val env)
          (succeed 'ok fail2)
        )
        fail
      )
    )
  )
)

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp)) (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc
        env
        (lambda (val fail2)
          (let ((old-val (lookup-variable-value var env)))
            (set-variable-value! var val env)
            (succeed
              'ok
              (lambda () (set-variable-value! var old-val env) (fail2))
            )
          )
        )
        fail
      )
    )
  )
)

;; apply
(define (analyze-application exp)
  (let ((f-proc (analyze (operator exp)))
        (a-procs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (f-proc
        env
        (lambda (proc fail2) ; proc: evaluated f-proc, only succeed get this
          (get-args
            a-procs
            env
            (lambda (args fail3) ; args: similar with above
              (execute-application proc args succeed fail3)
            )
            fail2
          )
        )
        fail
      )
    )
  )
)

(define (get-args procs env succeed fail)
  (if (null? procs)
    (succeed '() fail)
    ((car procs) ; <- eval first a-proc to get first arg
       env
       (lambda (arg fail2) ; if first arg succeed
         (get-args
           (cdr procs)
           env
           (lambda (args fail3) ; if rest args succeed (recursive)
             (succeed (cons arg args) fail3) ; then cons them together
           )
           fail2
         )
       )
       fail
    )
  )
)

(define (execute-application proc args succeed fail)
  (cond
    ((primitive-procedure? proc)
     (succeed (apply-primitive-procedure proc args) fail)
    )
    ((compound-procedure? proc)
     ((procedure-body proc)
        (extend-environment
          (procedure-parameters proc)
          args
          (procedure-environment proc)
        )
        succeed
        fail
     )
    )
    (else (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))
  )
)

;; amb
(define (analyze-amb exp)
  (let ((c-procs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices)
             env
             succeed
             (lambda () (try-next (cdr choices))) ; if not succeed, try next choice
          )
        )
      )
      (try-next c-procs)
    )
  )
)

;; driver
(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (cond
        ((eq? input 'quit) (quit))
        ((eq? input 'try-again) (try-again))
        (else
          (begin
           (newline)
           (display ";;; Starting a new problem ")
           (ambeval
             input
             the-global-environment
             (lambda (val next-alternative) ; next-alternative here occupies fail, which means other possibility
               (announce-output output-prompt)
               (user-print val)
               (internal-loop next-alternative)
               )
             (lambda () ; all fail, restart REPL
               (announce-output ";;; There are no more values of")
               (user-print input)
               (driver-loop)
               )
             )
          )
        )
      )
    )
  )
  (internal-loop
    (lambda ()
      (newline)
      (display ";;; There is no current problem")
      (driver-loop)
    )
  )
)

(if (not (defined? 'dont-run-amb))
  (driver-loop)
)
