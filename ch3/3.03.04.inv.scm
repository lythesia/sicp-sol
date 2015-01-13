(define (inverter input output)
  (define (inv-action-proc)
    (let ((new-val (logical-not (get-signal input))))
      ; (display "(inv) setout add to agenda[")(display (+ (current-time the-agenda) inverter-delay))(display "]")(newline)
      ; (after-delay inverter-delay (cons "(inv) set out-val" (lambda () (display "inv-result: ")(display new-val)(newline)(set-signal! output new-val))))
      (after-delay inverter-delay (lambda () (set-signal! output new-val)))
    )
  )
  ; (add-action! input (cons "inv-action" inv-action-proc))
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