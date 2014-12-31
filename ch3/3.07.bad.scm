(define (make-account balance passwd)
  (let ((passwd-list (list passwd)))
    (define (pass-match? pass)
      (define (iter rest)
        (cond
          ((null? rest) #f)
          ((eq? pass (car rest)) #t)
          (else (iter (cdr rest)))
        )
      )
      (iter passwd-list)
    )

    (define (withdraw x)
      (if (>= balance x) (begin (set! balance (- balance x)) balance)
        (error "Not enough balance -- WITHDRAW" (list balance x))
      )
    )

    (define (deposit x)
      (begin (set! balance (+ balance x)) balance)
    )

    (define (joint new-pass)
      (begin (set! passwd-list (cons new-pass passwd-list)) dispatch)
    )

    (define (dispatch pass op)
      (if (pass-match? pass)
        (cond
          ((eq? op 'withdraw) withdraw)
          ((eq? op 'deposit) deposit)
          ((eq? op 'joint) joint)
          (else (error "Illegal operation -- inner MAKE-ACCOUNT" op))
        )
        (lambda (x) "Incorrect password.")
      )
    )
    dispatch
  )
)
(define (make-joint acc passwd new-passwd)
  ((acc passwd 'joint) new-passwd)
)

; test
(define peter-acc (make-account 100 'open-sesame))
(display ((peter-acc 'open-sesame 'withdraw) 10))(newline) ; should 90
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
(display ((paul-acc 'rosebud 'withdraw) 10))(newline)  ; should 80
(display ((peter-acc 'open-sesame 'withdraw) 11))(newline) ; should 70
