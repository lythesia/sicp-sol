; (load "4.12.scm")
;; a)
(define (lookup-variable-value var env)
  (let ((ret (lookup-variable-pair var env)))
    (if ret
      (let ((val (cdr ret)))
        (if (eq? val '*unassigned*)
          (error "Unassigned variable" var)
          val
        )
      )
      (error "Unboud variable" var)
    )
  )
)

;; b)
; (define (f x)
;   (define (p1 ..)
;   )
;   (define (p2 ..)
;   )
;   ..
; )
(load "4.01.02.parse.scm")
(define (scan-out-defines body)
  (define (defs seq)
    (if (null? seq)
      '()
      (let ((expr (car seq)) (rest (cdr seq)))
        (if (definition? expr)
          (cons expr (defs rest))
          (defs rest)
        )
      )
    )
  )
  (define (non-defs seq)
    (if (null? seq)
      '()
      (let ((expr (car seq)) (rest (cdr seq)))
        (if (definition? expr)
          (non-defs rest)
          (cons expr (non-defs rest))
        )
      )
    )
  )
  ; let
  ; (
  ;   var-1 val1
  ;   var-2 val2
  ;   ..
  ; )
  ; (
  ;   (set-1 ..)
  ;   (set-2 ..)
  ;   .. others
  ; )
  (cons
    'let
    (cons
      (map (lambda (x) (list x '*unassigned*)) (map definition-variable (defs body)))
      (append
        (map (lambda (x) (cons 'set! (list (definition-variable x) (definition-value x)))) (defs body))
        (non-defs body)
      )
    )
  )
)

; test
(display
  (scan-out-defines
    '(
      (define x 1)
      (define y 2)
      (+ x y)
      (display x)
     )
  )
)

;; c)
;; better in `make-procedure`, check `apply` in 4.01.01.ea.scm:
;; ```
;;  ((compound-procedure? procedure)
;;    (eval-sequence
;;      (procedure-body procedure)
;;      (extend-environment                 ; do extend env
;;        (procedure-parameters procedure)  ; name param
;;        arguments                         ; real arg
;;        (procedure-environment procedure) ; parent env
;;      )
;;    )
;;  )
;; ```
;; `procedure-body` is called each time `eval-sequence` (or its upper: `apply`) called on same
;; procedure, thus cause the transformation of `scan-out-defines` processed many times.
;; while in `make-procedure`, the transformation is processed only once, after that each apply on
;; this procedure is evaluated on the transformed expressions.
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env)
)
