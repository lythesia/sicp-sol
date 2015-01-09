; load after `the-agenda` def and wire.scm

; `the-agenda`: to be processed action table
(define (after-delay delay-time action)
  ; (display (car action))(display " add to agenda @ ")(display (+ delay-time (current-time the-agenda)))(newline)
  (add-to-agenda! (+ delay-time (current-time the-agenda)) action the-agenda)
)

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      ; ((cdr first-item)) ; call action
      (first-item) ; call action
      (remove-first-agenda-item! the-agenda)
      ; (print-agenda the-agenda)
      (propagate)   ; to next
    )
  )
)

(define (probe name wire)
  (add-action!
    wire
    (lambda () ; action: simply print
      (newline)
      (display name)
      (display " @")
      (display (current-time the-agenda))
      (display " New-value = ")
      (display (get-signal wire))
    )
  )
)
