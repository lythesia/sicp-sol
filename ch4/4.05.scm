(load "4.01.02.parse.scm")

; override cond->if sub
(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses)) (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause is not last -- COND->IF" clauses)
        )
        ; [ref](https://wqzhang.wordpress.com/2009/10/12/sicp-exercise-4-5/#comment-1732)
        (if (cond-apply-form? first)
          (make-if
            (cond-apply-test first)
            (make-application (cond-apply-recipient first) (cond-apply-test first))
            (expand-clauses rest)
          )
        )
        (make-if
          (cond-predicate first)                ; pred
          (sequence->exp (cond-actions first))  ; cons
          (expand-clauses rest)                 ; alter
        )
      )
    )
  )
)
(define (cond-apply-form? exp) (eq? (cadr exp) '=>))
(define (cond-apply-test exp) (car exp))
(define (cond-apply-recipient exp) (caddr exp))

(define (make-application proc args) (list proc args))
