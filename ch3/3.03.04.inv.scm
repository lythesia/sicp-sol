(define (inverter input output)
  (define (inv-action-proc)
    (let ((new-val (logical-not (get-signal input))))
      (after-delay inverter-delay (lambda () (set-signal! output new-val)))
    )
  )
  (add-action! input inv-action-proc)
  'inv
)

(define (logical-not input)
  (cond
    ((= input 0) 1)
    ((= input 1) 0)
    (else (error "Invalid signal" s))
  )
)
