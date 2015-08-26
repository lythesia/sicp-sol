(include "util.scm")
(include "5.02.01.mac.scm")
;; disjoint set struct
(define (make-dataset)
  (let ((dataset '()))
    (define (adjoin! datum)
      (if (not (is-in-dataset? datum))
        (set! dataset (cons datum dataset))
      )
    )
    (define (is-in-dataset? datum)
      (cond
        ((symbol? datum) (memq datum dataset))
        ((list? datum) (member datum dataset))
        (else (error "Unknown data type -- DATASET" datum))
      )
    )
    (define (print) (display dataset)(newline))
    (define (dispatch message)
      (cond
        ((eq? message 'adjoin!) adjoin!)
        ((eq? message 'print) (print))
        (else (error "Unknown request -- DATASET" message))
      )
    )
    dispatch
  )
)
(define (insert-dataset! datum dataset) ((dataset 'adjoin!) datum))
(define (print-dataset dataset) (dataset 'print))

;; machine
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each
      (lambda (register-name) ((machine 'allocate-register) register-name))
      register-names
    )
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence) (assemble controller-text machine))
    machine
  )
)
(define make-new-machine-old make-new-machine)
(define (make-new-machine)
  (let
    ((base-machine (make-new-machine-old))
     (instruction-table
       (list
         (list 'assign  (make-dataset))
         (list 'test    (make-dataset))
         (list 'branch  (make-dataset))
         (list 'goto    (make-dataset))
         (list 'save    (make-dataset))
         (list 'restore (make-dataset))
       )
     )
     (spec-register-table
       (list
         (list 'goto    (make-dataset))
         (list 'save    (make-dataset))
         (list 'restore (make-dataset))
       )
     )
     (assign-register-table
       (list
         ; original has pc and flag
         (list 'pc (make-dataset))
         (list 'flag (make-dataset))
       )
     ))
    (define (allocate-register name)
      ((base-machine 'allocate-register) name) ; it will crash when duplicate
      (set! assign-register-table (cons (list name (make-dataset)) assign-register-table))
    )
    ; internal use
    (define (lookup-dataset-in-table name table)
      (let ((val (assoc name table)))
        (if val (cadr val) (error "entry not found -- LOOKUP-DATASET" name table))
      )
    )
    (define (lookup-assign-register name) (lookup-dataset-in-table name assign-register-table))
    (define (lookup-spec-register name) (lookup-dataset-in-table name spec-register-table))
    (define (lookup-instruction-set name) (lookup-dataset-in-table name instruction-table))
    (define (print-dataset-table table title)
      (newline)(display title)(newline)
      (for-each (lambda (entry) (display (car entry)) (display ": ") (print-dataset (cadr entry))) table)
    )
    (define (print-all-datasets)
      (print-dataset-table instruction-table "Instructions")
      (print-dataset-table spec-register-table "Special Register use")
      (print-dataset-table assign-register-table "Assignment")
    )
    (define (dispatch message)
      (cond
        ; override
        ((eq? message 'allocate-register) allocate-register)
        ; added
        ((eq? message 'print-all-datasets) (print-all-datasets))
        ((eq? message 'lookup-instruction-set) lookup-instruction-set)
        ((eq? message 'lookup-spec-register) lookup-spec-register)
        ((eq? message 'lookup-assign-register) lookup-assign-register)
        ; base
        (else (base-machine message))
      )
    )
    dispatch
  )
)

(include "5.02.02.as.scm")
(include "5.02.03.inst-proc.scm")
(define make-assign-old make-assign)
(define (make-assign inst machine labels ops pc)
  (let ((dataset ((machine 'lookup-assign-register) (assign-reg-name inst))))
    (insert-dataset! (assign-value-proc inst) dataset)
    (make-assign-old inst machine labels ops pc)
  )
)

(define make-goto-old make-goto)
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (if (register-exp? dest) ; only insert those who target is register
      (let ((dataset ((machine 'lookup-spec-register) 'goto)))
        (insert-dataset! (register-exp-reg dest) dataset)
      )
    )
  )
  (make-goto-old inst machine labels pc)
)

(define make-save-old make-save)
(define (make-save inst machine stack pc)
  (let ((dataset ((machine 'lookup-spec-register) 'save)))
    (insert-dataset! (stack-inst-reg-name inst) dataset)
  )
  (make-save-old inst machine stack pc)
)

(define make-restore-old make-restore)
(define (make-restore inst machine stack pc)
  (let ((dataset ((machine 'lookup-spec-register) 'restore)))
    (insert-dataset! (stack-inst-reg-name inst) dataset)
  )
  (make-restore-old inst machine stack pc)
)

(define make-execution-procedure-old make-execution-procedure)
(define (make-execution-procedure inst labels machine pc flag stack ops)
  (let ((dataset ((machine 'lookup-instruction-set) (car inst))))
    (insert-dataset! inst dataset)
  )
  (make-execution-procedure-old inst labels machine pc flag stack ops)
)


;; test
(define fib-machine (make-machine ;register-names ops controller-text
    '(n val continue)
    (list (list '< <) (list '- -) (list '+ +))
    '(  ; from ch5.scm
           (assign continue (label fib-done))
         fib-loop
           (test (op <) (reg n) (const 2))
           (branch (label immediate-answer))
           ;; set up to compute Fib(n-1)
           (save continue)
           (assign continue (label afterfib-n-1))
           (save n)                           ; save old value of n
           (assign n (op -) (reg n) (const 1)); clobber n to n-1
           (goto (label fib-loop))            ; perform recursive call
         afterfib-n-1                         ; upon return, val contains Fib(n-1)
           (restore n)
           (restore continue)
           ;; set up to compute Fib(n-2)
           (assign n (op -) (reg n) (const 2))
           (save continue)
           (assign continue (label afterfib-n-2))
           (save val)                         ; save Fib(n-1)
           (goto (label fib-loop))
         afterfib-n-2                         ; upon return, val contains Fib(n-2)
           (assign n (reg val))               ; n now contains Fib(n-2)
           (restore val)                      ; val now contains Fib(n-1)
           (restore continue)
           (assign val                        ; Fib(n-1)+Fib(n-2)
                   (op +) (reg val) (reg n)) 
           (goto (reg continue))              ; return to caller, answer is in val
         immediate-answer
           (assign val (reg n))               ; base case: Fib(n)=n
           (goto (reg continue))
         fib-done)))

(fib-machine 'print-all-datasets)
