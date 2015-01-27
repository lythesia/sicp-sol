(load "4.03.scm")

; a)
(define (eval-and exp env)
  (define (eval-and-ops ops)
    (cond
      ((null? ops) #t)
      ((true? (eval (car ops) env)) (eval-and-ops (cdr ops)))
      (else #f)
    )
  )
)
(define (and? exp) (tagged-list? exp 'and))
(define (and-predicates exp) (cdr exp))

(define (eval-or exp env)
  (define (eval-or-ops ops)
    (cond
      ((null? ops) #f)
      ((true? (eval (car ops) env)) #t)
      (else (eval-or-ops (cdr ops)))
    )
  )
)
(define (or? exp) (tagged-list? exp 'or))
(define (or-predicates exp) (cdr exp))

(put 'and eval-and)
(put 'or eval-or)

; b)
(define (expand-and-clauses clauses)
  (if (null? clauses)
    'true
    (make-if (car clauses) (expand-and-clauses (cdr clauses)) 'false)
  )
)
(define (and->if exp) (expand-and-clauses (and-predicates exp)))

(define (expand-or-clauses clauses)
  (if (null? clauses)
    'false
    (make-if (car clauses) 'true (expand-or-clauses (cdr clauses)))
  )
)
(define (or->if exp) (expand-or-clauses (or-predicates exp)))

(define (eval-and exp env) (eval (and->if exp) env))
(define (eval-or exp env) (eval (or->if exp) env))
