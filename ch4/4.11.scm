(load "4.01.03.env.scm")
; list-struct:
;   (var-1 . val-1) ..
(define (make-frame vars vals) (cons 'var-list (map cons vars vals)))
(define (frame-pairs f) (cdr f))
(define (add-binding-to-frame! var val f)
  (set-cdr! f (cons (cons var val) (frame-pairs f)))
)
; O(n)
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan lst)
      (cond
        ((null? lst) (env-loop (enclosing-environment env)))
        ((eq? var (caar lst)) (cdar lst))
        (else (scan (cdr lst)))
      )
    )
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan (frame-pairs (first-frame env)))
    )
  )
  (env-loop env)
)
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan lst)
      (cond
        ((null? lst) (env-loop (enclosing-environment env)))
        ((eq? var (caar lst)) (set-cdr! (car lst) val))
        (else (scan (cdr lst)))
      )
    )
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET" var)
      (scan (frame-pairs (first-frame env)))
    )
  )
  (env-loop env)
)
(define (define-variable! var val env)
  (let ((f (first-frame env)))
    (define (scan lst)
      (cond
        ((null? vars) (add-binding-to-frame! var val f))
        ((eq? var (caar lst)) (set-cdr! (car lst) val))
        (else (scan (cdr lst)))
      )
    )
    (scan (frame-pairs f))
  )
)
