(define dont-run 1)
(include "4.01.no-aly.scm")

(define (eval exp env)
  (cond
    ; base {{
    ; self-eval
    ((self-evaluating? exp) exp)

    ; lookup var
    ((variable? exp) (lookup-variable-value exp env))
    ; }}

    ; special {{
    ; quotation
    ((quoted? exp) (text-of-quotation exp))

    ; assignment/define
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))

    ; if
    ((if? exp) (eval-if exp env))

    ; lambda
    ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))

    ; begin
    ((begin? exp) (eval-sequence (begin-actions exp) env))

    ; cond
    ((cond? exp) (eval (cond->if exp) env))

    ; let
    ((let? exp) (eval (let->combination exp) env))

    ; apply
    ((application? exp) (apply (actual-value (operator exp) env) (operands exp) env)) ; <- operator is `actual-value` handeld? yes, since itself can a procedure returned from a compound procedure
    ; }}

    ; unhandled
    (else (error "Unknown expression type -- EVAL" exp))
  )
)

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)
  )
)

(define (apply procedure arguments env) ; <- env is needed
  (cond
    ; simple
    ((primitive-procedure? procedure)
     (apply-primitive-procedure
       procedure
       (list-of-arg-values arguments env)
     )
    )

    ; compound
    ((compound-procedure? procedure)
      (eval-sequence
        (procedure-body procedure)
        (extend-environment
          (procedure-parameters procedure)
          (list-of-delayed-or-not-args (procedure-parameter-tags procedure) arguments env)
          (procedure-environment procedure)
        )
      )
    )

    ; unhandled
    (else (error "Unknown procedure type -- APPLY" procedure))
  )
)

;; override
(define (procedure-parameters proc)
  (define (extract p)
    (if (pair? p)
      (car p)
      p
    )
  )
  (map extract (cadr proc))
)
(define (procedure-parameter-tags proc)
  (define (tag p)
    (if (pair? p)
      (cadr p)
      'normal
    )
  )
  (map tag (cadr proc))
)

(define (list-of-arg-values args env)
  (map (lambda (arg) (actual-value arg env)) args)
)
(define (list-of-delayed-or-not-args tags args env)
  (map (lambda (tag arg) (delay-or-not tag arg env)) tags args)
)

; lazy struct
(define (delay-or-not tag exp env)
  (cond
    ((eq? tag 'normal) (actual-value exp env))
    ((eq? tag 'lazy) (list 'simple-thunk exp env))
    ((eq? tag 'lazy-memo) (list 'thunk exp env))
    (else (error "Unknown tag type -- DELAY-OR-NOT" tag))
  )
)

(define (simple-thunk? obj) (tagged-list? obj 'simple-thunk))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define thunk-exp cadr)
(define thunk-env caddr)
(define thunk-value cadr)

(define (actual-value exp env)
  (force-it (eval exp env))
)

(define (force-it obj)
  (cond
    ((simple-thunk? obj)
     (actual-value (thunk-exp obj) (thunk-env obj))
    )
    ((thunk? obj)
     (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
       ; 'evaluated-thunk <real-value> '()
       (set-car! obj 'evaluated-thunk)
       (set-car! (cdr obj) result)
       (set-cdr! (cdr obj) '())
       result
     )
    )
    ((evaluated-thunk? obj)
     (thunk-value obj)
    )
    (else obj)
  )
)

;; driver
(define input-prompt ";;; L-EVAL input:")
(define output-prompt ";;; L-EVAL output:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read))) ; read wait for input
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)
    )
  )
  (if (= exit-flag 0)
    (driver-loop)
  )
)

(if (not (defined? 'dont-run-lazy)) (driver-loop))
