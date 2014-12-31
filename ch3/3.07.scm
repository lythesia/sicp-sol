(load "3.03.scm")

; awesome!
(define (make-joint acc org-pass new-pass)
  (lambda (pass op)
    (if (eq? pass new-pass) (acc org-pass op) ; orginal account as a PROXY to get the right operation!
      (lambda (useless-arg) "Incorrect password.")
    )
  )
)

; test
; (define peter-acc (make-account 100 'open-sesame))
; (display ((peter-acc 'open-sesame 'withdraw) 10))(newline) ; should 90
; (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
; (display ((paul-acc 'rosebud 'withdraw) 10))(newline)  ; should 80
; (display ((peter-acc 'open-sesame 'withdraw) 10))(newline) ; should 70
