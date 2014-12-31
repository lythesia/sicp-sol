(define (make-account balance passwd)
  (define (withdraw x)
    (if (>= balance x) (begin (set! balance (- balance x)) balance)
      (error "Not enough balance -- WITHDRAW" (list balance x))
    )
  )
  (define (deposit x)
    (begin (set! balance (+ balance x)) balance)
  )

  (define (passwd-match? pass) (eq? pass passwd))

  (define (dispatch pass op)
    (if (passwd-match? pass)
      (cond
        ((eq? op 'withdraw) withdraw)
        ((eq? op 'deposit) deposit)
        (else (error "Illegal operation -- inner MAKE-ACCOUNT" op))
      )
      (lambda (useless-arg) "Incorrect password.")
    )
  )
  dispatch
)

; test
; (define acc (make-account 100 'secret))
; (display ((acc 'secret 'withdraw) 30))(newline)
; (display ((acc 'wrong 'deposit) 50))(newline)
