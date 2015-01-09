; generic dispatch
(define (inform-about-value constraint) (constraint 'have-value))
(define (inform-about-no-value constraint) (constraint 'lost-value))

; `me` to control the **SINGLE-DIRECTION BROADCAST**
; const constraint
(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request)
  )
  (connect connector me)
  (set-value! connector value me)
  me
)

; adder constraint
(define (adder a1 a2 sum)
  ; work like dispatch
  (define (process-new-value)
    (cond
      ((and (has-value? a1) (has-value? a2)) (set-value! sum (+ (get-value a1) (get-value a2)) me))
      ((and (has-value? a1) (has-value? sum)) (set-value! a2 (- (get-value sum) (get-value a1)) me))
      ((and (has-value? a2) (has-value? sum)) (set-value! a1 (- (get-value sum) (get-value a2)) me))
    )
  )
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value)
  )
  (define (me request)
    (cond
      ((eq? request 'have-value) (process-new-value))
      ((eq? request 'lost-value) (process-forget-value))
      (else (error "Unknown request -- ADDER" request))
    )
  )

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me
)

; multiplier constraint
(define (multiplier m1 m2 product)
  ; work like dispatch
  (define (process-new-value)
    (cond
      ((or (and (has-value? m1) (zero? (get-value m1))) (and (has-value? m2) (zero? (get-value m2)))) (set-value! product 0 me))
      ((and (has-value? m1) (has-value? m2)) (set-value! product (* (get-value m1) (get-value m2)) me))
      ((and (has-value? m1) (has-value? product)) (set-value! m2 (/ (get-value product) (get-value m1)) me))
      ((and (has-value? m2) (has-value? product)) (set-value! m1 (/ (get-value product) (get-value m2)) me))
    )
  )
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value)
  )
  (define (me request)
    (cond
      ((eq? request 'have-value) (process-new-value))
      ((eq? request 'lost-value) (process-forget-value))
      (else (error "Unknown request -- ADDER" request))
    )
  )

  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me
)
