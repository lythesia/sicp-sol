;; a)
; letrec ((v1 e1) (v2 e2) ..)
; body

; (define (eval exp env)
;   ; ..
;   ((letrec? exp) (eval (letrec->let exp) env))
;   ; ..
; )

(define (letrec? exp)
  (tagged-list? exp 'letrec)
)

(load "4.06.scm")
; let ((v1 '*unassigned*) (..))
; set! v1 e1
; ..
; body
(define (letrec->let exp)
  (let* ((bindings (let-bindings exp))
         (let-vars (let-variables bindings))
         (let-vals (let-values bindings))
        )
    (cons
      'let
      (cons
        (letrec-lets let-vars)  ; lets
        (append (letrec-sets let-vars let-vals) (let-body exp)) ; sets body
      )
    )
  )
)

(define (letrec-lets vars)
  (map
    (lambda (var) (list var '*unassigned*))
    vars
  )
)

(define (letrec-sets vars vals)
  (map
    (lambda (var val) (list 'set! var val))
    vars
    vals
  )
)

; test
(display
  (letrec->let
    '(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
              (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
       (even? x)
     )
  )
)

;; b)
; Louis version
; (let ((even? (lambda (n) (..))) (odd? (lambda (n) (..))))
;   <body>
; )
;
; converting to:
; ((lambda (even? odd?) <body>)
;   (lambda (n) (..))
;   (lambda (n) (..))
; )
;
; see (f 5)
;
;    +------+
; E1 | x: 5 |
;    +------+
;        ^
;        |
;        |
;    +---+-------------------------+
; E2 | even? (proc param body env) | both env is pointed to upper env(which has no even? or odd?), when even?
;    | odd? (proc param body env)  | invoked, it cannot find odd? in that env!
;    +-----------------------------+
;
