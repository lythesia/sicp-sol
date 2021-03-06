(define (make-execution-procedure inst labels machine pc flag stack ops)
  (let ((op (car inst)))
    (cond
      ((eq? op 'assign) (make-assign inst machine labels ops pc))   ; need `op` to compute
      ((eq? op 'test) (make-test inst machine labels ops flag pc))  ; need `op` to set `flag`
      ((eq? op 'branch) (make-branch inst machine labels flag pc))  ; need check `flag`
      ((eq? op 'goto) (make-goto inst machine labels pc))
      ((eq? op 'save) (make-save inst machine stack pc))
      ((eq? op 'restore) (make-restore inst machine stack pc))
      ((eq? op 'perform) (make-perform inst machine labels ops pc))
      (else (error "Unknown instruction type -- ASSEMBLE" op))
    )
  )
)
; (assign <target> proc...)
(define (make-assign inst machine labels ops pc)
  (let ((target (get-register machine (assign-reg-name inst))) (value-exp (assign-value-proc inst)))
    (let
      ((value-proc
         (if (operation-exp? value-exp)
           (make-operation-exp value-exp machine labels ops)
           (make-primitive-exp (car value-exp) machine labels)
         )
       ))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)
      )
    )
  )
)
(define assign-reg-name cadr)
(define assign-value-proc cddr)
; (const ..)
; (label ..)
; (reg ..)
(define (make-primitive-exp exp machine labels)
  (cond
    ((constant-exp? exp)
     (let ((c (constant-exp-value exp))) (lambda () c)))
    ((label-exp? exp)
     (let ((insts (lookup-label labels (label-exp-label exp)))) (lambda () insts)))
    ((register-exp? exp)
     (let ((r (get-register machine (register-exp-reg exp)))) (lambda () (get-contents r))))
    (else (error "Unknown expression type -- ASSEMBLE" exp))
  )
)
(define (constant-exp? exp) (tagged-list? exp 'const))
(define constant-exp-value cadr)
(define (label-exp? exp) (tagged-list? exp 'label))
(define label-exp-label cadr)
(define (register-exp? exp) (tagged-list? exp 'reg))
(define register-exp-reg cadr)
; (op ..) [operands]
; (define (make-operation-exp exp machine lables operations)
;   (let
;     ((op (lookup-operation (operation-exp-op exp) operations))
;      (a-procs (map (lambda (e) (make-primitive-exp e machine labels)) (operation-exp-operands exp))))
;     (lambda () (apply op (map (lambda (p) (p)) a-procs)))
;   )
; )
;; 5.09.scm
(define (make-operation-exp exp machine labels operations)
  (let
    ((op (lookup-operation (operation-exp-op exp) operations))
     (a-procs
       (map
         (lambda (e)
           (if (label-exp? e)
             (error "cannot operate on label -- MAKE-OPERATION-EXP" e)
             (make-primitive-exp e machine labels))
           )
         (operation-exp-operands exp)
       )
     ))
    (lambda () (apply op (map (lambda (p) (p)) a-procs)))
  )
)
(define (operation-exp? exp) (and (pair? exp) (tagged-list? (car exp) 'op)))
(define operation-exp-op cadar)
(define operation-exp-operands cdr)
; (list 'op operation)
(define (lookup-operation symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
      (cadr val)
      (error "Unknown operation -- ASSEMBLE" symbol)
    )
  )
)
(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc)))
)

; (test (op ..) [operands])
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
      (let ((condition-proc (make-operation-exp condition machine labels operations)))
        (lambda () (set-contents! flag (condition-proc)) (advance-pc pc))
      )
      (error "Bad TEST instruction -- ASSEMBLE" inst) ; must be (op ..) [operands]
    )
  )
)
(define test-condition cdr)

; (branch (label ..))
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
      (let ((insts (lookup-label labels (label-exp-label dest)))) ; 1. why lookup-label gives all insts(behind) here?
        (lambda ()
          (if (get-contents flag)
            (set-contents! pc insts)  ; 2. since here we use it to set pc
            (advance-pc pc)
          )
        )
      )
      (error "Bad BRANCH instruction -- ASSEMBLE" inst)
    )
  )
)
(define branch-dest cadr)

; (goto (label/reg ..))
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond
      ((label-exp? dest)
       (let ((insts (lookup-label labels (label-exp-label dest))))
         (lambda () (set-contents! pc insts))
       ))
      ((register-exp? dest)
       (let ((reg (get-register machine (register-exp-reg dest))))
         (lambda () (set-contents! pc (get-contents reg)))
       ))
      (error "Bad GOTO instruction -- ASSEMBLE" inst)
    )
  )
)
(define goto-dest cadr)

; (save ..)
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc)
    )
  )
)
; (restore ..)
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc)
    )
  )
)
(define stack-inst-reg-name cadr)

; (perform (op ..) [operands])
(define (make-perform inst machine labels ops pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
      (let ((action-proc (make-operation-exp action machine labels ops)))
        (lambda () (action-proc) (advance-pc pc))
      )
      (error "Bad PERFORM instruction -- ASSEMBLE" inst)
    )
  )
)
(define perform-action cdr)
