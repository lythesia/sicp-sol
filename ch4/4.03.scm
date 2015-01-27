(load "4.01.01.scm")

(define eval-table (make-hash-table))
(define (get (lambda (k) (hash-ref eval-table k))))
(define (put (lambda (k v) (hash-set! eval-table k v))))

; install
(put 'quote (lambda (exp env) (text-of-quotation exp))) ; since env not used
(put 'set! eval-assignment)
(put 'define eval-assignment)
(put 'if eval-if)
(put 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
(put 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put 'cond (lambda (exp env) (eval (cond->if exp) env)))

(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((get (car exp)) ((get (car exp)) exp env)) ; must in form of `(xx exp env)`
    ((application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
    (else (error "Unknown expression type -- EVAL" exp))
  )
)
