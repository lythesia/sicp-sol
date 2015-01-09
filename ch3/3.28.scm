(define (or-gate a1 a2 output)
  (define (or-action-proc)
    (let ((new-val (logical-or (get-signal a1) (get-signal a2)))) ; get-signal has the action to process
      ; (display "(or) setout add to agenda[")(display (+ (current-time the-agenda) or-gate-delay))(display "]")(newline)
      ; (after-delay or-gate-delay (cons "(or) set-out" (lambda () (display "or-result: ")(display new-val)(newline)(set-signal! output new-val))))
      (after-delay or-gate-delay (lambda () (set-signal! output new-val)))
    )
  )

  ; bind action on wire
  ; so output set twice: a1 and a2 on first time
  ; (add-action! a1 (cons "or-action" or-action-proc))
  ; (add-action! a2 (cons "or-action" or-action-proc))
  (add-action! a1 or-action-proc) ; prepend && action invoked!
  (add-action! a2 or-action-proc) ; onaji
  'or
)
(define (logical-or a1 a2)
  (cond
    ((or (= 1 a1) (= 1 a2)) 1)
    (else 0)
  )
)
