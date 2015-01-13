(load "3.03.05.syn.scm")

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
        (error "squarer < 0 -- SQUARER" (get-value b))
        (set-value! a (sqrt (get-value b)) me)
      )
      (if (has-value? a)
        (set-value! b (expt (get-value a) 2) me)
      )
    )
  )
  (define (process-forget-value)
    (forget-value! b me)
    (forget-value! a me)
    (process-new-value)
  )
  (define (me request)
    (cond
      ((eq? request 'have-value) (process-new-value))
      ((eq? request 'lost-value) (process-forget-value))
      (else (error "Unknown request -- SQUARER" request))
    )
  )
  (connect a me)
  (connect b me)
  me
)

; test
(define A (make-connector))
(define B (make-connector))
(squarer A B)

(probe "A" A)
(probe "B" B)

(set-value! A 3 'user)
; 9

(forget-value! A 'user)
(set-value! B 2.25 'user)
; 1.5
