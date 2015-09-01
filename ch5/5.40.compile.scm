;; add compile-time-env based on 5.05.compile.scm

(include "5.05.syntax.scm")

(define (compile exp target linkage compile-time-env)
  (cond
    ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
    ((quoted? exp) (compile-quoted exp target linkage))
    ((variable? exp) (compile-variable exp target linkage compile-time-env))
    ((assignment? exp) (compile-assignment exp target linkage))
    ((definition? exp) (compile-definition exp target linkage compile-time-env))
    ((if? exp) (compile-if exp target linkage compile-time-env))
    ((lambda? exp) (compile-lambda exp target linkage compile-time-env))
    ((begin? exp) (compile-sequence (begin-actions exp) target linkage compile-time-env))
    ((cond? exp) (compile (cond->if exp) target linkage compile-time-env))
    ((application? exp) (compile-application exp target linkage compile-time-env))
    (else (error "Unknown expression type -- COMPILE" exp))
  )
)

;; generate isnt
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements)
)
(define (empty-instruction-sequence) (make-instruction-sequence '() '() '()))

;; generate branch inst with linkage
(define (compile-linkage linkage)
  (cond
    ; return to upper level
    ((eq? linkage 'return) (make-instruction-sequence '(continue) '() '((goto (reg continue)))))
    ; next inst
    ((eq? linkage 'next) (empty-instruction-sequence))
    ; got label (linkage is the label)
    (else (make-instruction-sequence '() '() `((goto (label ,linkage)))))
  )
)

; link code sequences accordingly (sometimes we dont need to save continue)
; give out instruction sequence
(define (end-with-linkage linkage inst-seq)
  (preserving '(continue) inst-seq (compile-linkage linkage))
)
;; (preserving <list-of-preserved> <inst-sequence> <after-inst-sequence>)
;; after <inst-sequence>, preserved reg is restored, then we do things after

;; detailed compile
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
    linkage
    (make-instruction-sequence '() (list target) `((assign ,target (const ,exp))))
  )
)

(define (compile-quoted exp target linkage)
  (end-with-linkage
    linkage
    (make-instruction-sequence '() (list target) `((assign ,target (const ,(text-of-quotation exp)))))
  )
)

(define (compile-variable exp target linkage compile-time-env)
  (end-with-linkage
    linkage
    (make-instruction-sequence '(env) (list target) `((assign ,target (op lookup-variable-value) (const ,exp) (reg env))))
  )
)

(define (compile-assignment exp target linkage compile-time-env)
  (let ((var (assignment-variable exp)) (get-value-code (compile (assignment-value exp) 'val 'next compile-time-env)))
    (end-with-linkage
      linkage
      (preserving
        '(env)          ; if we preserve `val` here
        get-value-code  ; then after this, wrong `val` will be restored, which covers that from `assignment-value`
        (make-instruction-sequence '(env val) (list target)
          `((perform (op set-variable-value!) (const ,var) (reg val) (reg env))
            (assign ,target (const ok))
           )
        )
      )
    )
  )
)

(define (compile-definition exp target linkage compile-time-env)
  (let ((var (definition-variable exp)) (get-value-code (compile (definition-value exp) 'val 'next compile-time-env)))
    (end-with-linkage
      linkage
      (preserving
        '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
          `((perform (op define-variable!) (const ,var) (reg val) (reg env))
            (assign ,target (const ok))
           )
        )
      )
    )
  )
)

; <compile predicate, result to `val`, linkage `next`>  ; (1)
; (test (op false?) (reg val))
; (branch (label false-branch))
;true-branch
; <compile if-predicate>
;false-branch
; <compile if-alternative>
;after-if
;; note that labels should generate in unique
(define (compile-if exp target linkage compile-time-env)
  (let ((true-br (make-label 'true-branch)) (false-br (make-label 'false-branch)) (after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage))) ; if after consequent is next, then we go after `after-if`
      (let ((p-code (compile (if-predicate exp) 'val 'next compile-time-env))  ; match (1)
            (c-code (compile (if-consequent exp) target consequent-linkage compile-time-env))
            (a-code (compile (if-alternative exp) target linkage compile-time-env))) ; whether linkage is 'next or not, still linkage so we dont need `let binding` like consequent-linkage 
        (preserving
          '(env continue)
          p-code
          (append-instruction-sequences
            (make-instruction-sequence '(val) '() `((test (op false?) (reg val)) (branch (label ,false-br))))
            (parallel-instruction-sequences
              (append-instruction-sequences true-br c-code)
              (append-instruction-sequences false-br a-code)
            )
            after-if
          )
        )
      )
    )
  )
)

(define (compile-sequence exp target linkage compile-time-env)
  (if (last-exp? exp)
    (compile (first-exp exp) target linkage compile-time-env)
    (preserving
      '(env continue)
      (compile (first-exp exp) target 'next compile-time-env)
      (compile-sequence (rest-exps exp) target linkage compile-time-env)
    )
  )
)

; <make-proc, proc->reg>
; <goto linkage or goto (label after-lambda)>
; <code-of-lambda-body> though we dont run it
; after-lambda
(define (compile-lambda exp target linkage compile-time-env)
  (let ((proc-entry (make-label 'entry)) (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage lambda-linkage (make-instruction-sequence '(env) (list target) `((assign ,target (op make-compiled-procedure) (label ,proc-entry) (reg env)))))
          (compile-lambda-body exp proc-entry compile-time-env)
        )
        after-lambda
      )
    )
  )
)
(define (compile-lambda-body exp proc-entry compile-time-env)
  (let ((formals (lambda-parameters exp)))  ; formal parameters
    (append-instruction-sequences
      (make-instruction-sequence '(env proc argl) '(env)
        `(,proc-entry
           (assign env (op compiled-procedure-env) (reg proc))
           (assign env (op extend-environment) (const ,formals) (reg argl) (reg env))
         )
      )
      (compile-sequence (lambda-body exp) 'val 'return compile-time-env)
    )
  )
)

; <compiled-operator -> proc, goto next>
; <compiled-operands -> argl>
; <compile call>
(define (compile-application exp target linkage compile-time-env)
  (let ((proc-code (compile (operator exp) 'proc 'next compile-time-env))
        (operand-codes (map (lambda (operand) (compile operand 'val 'next compile-time-env)) (operands exp))))
    (preserving
      '(env continue)
      proc-code
      (preserving '(proc continue) (construct-arglist operand-codes) (compile-procedure-call target linkage))
    )
  )
)
; construct-arglist reversely
(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
      (make-instruction-sequence '() '(argl) '((assign argl (const ()))))
      (let ((code-to-get-last-arg
              (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl) '((assign argl (op list) (reg val))))
              )))
        (if (null? (cdr operand-codes))
          code-to-get-last-arg
          (preserving '(env) code-to-get-last-arg (code-to-get-rest-args (cdr operand-codes)))
        )
      )
    )
  )
)
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving '(argl) (car operand-codes) (make-instruction-sequence '(val argl) '(argl) '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
      code-for-next-arg
      (preserving '(env) code-for-next-arg (code-to-get-rest-args (cdr operand-codes)))
    )
  )
)

(define (compile-procedure-call target linkage)
  (let ((primitive-br (make-label 'primitive-branch)) (compiled-br (make-label 'compiled-branch)) (after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
        (make-instruction-sequence '(proc) '()
          `((test (op primitive-procedure?) (reg proc))
            (branch (label ,primitive-br)))
        )
        (parallel-instruction-sequences
          (append-instruction-sequences
            compiled-br
            (compile-proc-appl target compiled-linkage)
          )
          (append-instruction-sequences
            primitive-br
            (end-with-linkage linkage (make-instruction-sequence '(proc argl) (list target) `((assign ,target (op apply-primitive-procedure) (reg proc) (reg argl)))))
          )
        )
        after-call
      )
    )
  )
)
; 4 cases respecting to target(val or not), linkage(return or not)
(define (compile-proc-appl target linkage)
  (cond
    ; a) val, not return: directly goto linkage
    ((and (eq? target 'val) (not (eq? linkage 'return)))
     (make-instruction-sequence '(proc) all-regs
      `((assign continue (label ,linkage))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))))
    )
    ; b) not val, not return: back to target <- val, then goto linkage
    ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
     (let ((proc-return (make-label 'proc-return)))
       (make-instruction-sequence '(proc) all-regs
        `((assign continue (label ,proc-return))
          (assign val (op compiled-procedure-entry) (reg proc))
          (goto (reg val))
          ,proc-return
          (assign ,target (reg val))
          (goto (label ,linkage))))
     )
    )
    ; c) val, return: after call directly (goto (reg continue)), this allows tail-rec
    ((and (eq? target 'val) (eq? linkage 'return))
     (make-instruction-sequence '(proc continue) all-regs
      `((assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))))
    )
    ; d) not val, return: error
    ((and (not (eq? target 'val)) (eq? linkage 'return))
     (error "return linkage, target not val -- COMPILE" target)
    )
  )
)

;;; initial/helper
;; label
(define label-counter 0)

(define (new-label-counter)
  (set! label-counter (+ label-counter 1))
  label-counter
)
(define (make-label name)
  (string->symbol (string-append (symbol->string name) (number->string (new-label-counter))))
)

;; reg
(define all-regs '(env proc val argl continue))
; for (make-instruction-sequence ..) use
; symbol? to treat instruction and label same (latter is symbol, so we use '() cause no satisfy)
(define (registers-needed s) (if (symbol? s) '() (car s)))
(define (registers-modified s) (if (symbol? s) '() (cadr s)))
(define (statements s) (if (symbol? s) (list s) (caddr s)))
(define (needs-register? inst-seq reg) (memq reg (registers-needed inst-seq)))
(define (modifies-register? inst-seq reg) (memq reg (registers-modified inst-seq)))

;; instruction
; util
(define (list-union s1 s2)
  (cond
    ((null? s1) s2)
    ((memq (car s1) s2) (list-union (cdr s1) s2))
    (else (cons (car s1) (list-union (cdr s1) s2)))
  )
)
; in s1, but not in s2
(define (list-difference s1 s2)
  (cond
    ((null? s1) '())
    ((memq (car s1) s2) (list-difference (cdr s1) s2))
    (else (cons (car s1) (list-difference (cdr s1) s2)))
  )
)
; result as instruction sequence: list of (reg-need reg-modify statements)
(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union (registers-needed seq1) (list-difference (registers-needed seq2) (registers-modified seq1)))
      (list-union (registers-modified seq1) (registers-modified seq2))
      (append (statements seq1) (statements seq2))
    )
  )
  (define (append-seq-list seqs)
    (if (null? seqs)
      (empty-instruction-sequence)
      (append-2-sequences (car seqs) (append-seq-list (cdr seqs)))
    )
  )
  (append-seq-list seqs)
)

; preseving
(define (preserving regs seq-to-wrap seq-after)
  (if (null? regs)
    (append-instruction-sequences seq-to-wrap seq-after)
    (let ((first-reg (car regs)))
      ; if register used in seq-after will be modified in seq-to-wrap, we protect it
      (if (and (needs-register? seq-after first-reg) (modifies-register? seq-to-wrap first-reg))
        (preserving
          (cdr regs)
          (make-instruction-sequence
            (list-union (list first-reg) (registers-needed seq-to-wrap))  ; since save is as need?
            (list-difference (registers-modified seq-to-wrap) (list first-reg))
            ; (append _ _ _)
            (append `((save ,first-reg)) (statements seq-to-wrap) `((restore ,first-reg)))
          )
          seq-after
        )
        (preserving (cdr regs) seq-to-wrap seq-after)
      )
    )
  )
)

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-modified seq)
    (append (statements seq) (statements body-seq))
  )
)

; only one of the two is chosed, so we dont need to proctect registers (like in "true" sequences)
(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
    (list-union (registers-needed seq1) (registers-needed seq2))
    (list-union (registers-modified seq1) (registers-modified seq2))
    (append (statements seq1) (statements seq2))
  )
)

;; procedure
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env)
)
(define (compiled-procedure? proc) (tagged-list? 'compiled-procedure? proc))
(define compiled-procedure-entry cadr)
(define compiled-procedure-env caddr)

;; compile-time-env
(define the-empty-compile-environment '())
(define (find-variable var compile-time-env)
  (define (env-loop envs e)
    (define (scan vars v)
      (cond
        ((null? vars) (env-loop (cdr envs) (+ e 1)))
        ((eq? var (car vars)) (list e v))
        (else (scan (cdr vars) (+ v 1)))
      )
    )
    (if (null? envs)
      'not-found
      (scan (car envs) 0)
    )
  )
  (env-loop compile-time-env 0)
)
