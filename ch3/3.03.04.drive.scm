; load after `the-agenda` def and wire.scm

; `the-agenda`: to be processed action table
(define (after-delay delay-time action)
  (add-to-agenda! (+ delay-time (current-time the-agenda)) action the-agenda)
)

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item) ; call action
      (remove-first-agenda-item! the-agenda)
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
      (display " ")
      (display (curren-time the-agenda))
      (display " New-value = ")
      (display (get-signal wire))
    )
  )
)
