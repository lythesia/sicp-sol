(define (cc amount coins)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (no-more? coins)) 0)
    (else
      (+
        (cc amount (except-first-coin-type coins))
        (cc (- amount (first-coin-val coins)) coins)
      )
    )
  )
)

(define (no-more? l) (null? l))
(define (except-first-coin-type cs) (cdr cs))
(define (first-coin-val cs) (car cs))

; (define us-coins (list 50 25 10 5 1))
; (define us-coins (list 25 50 10 1 5)) ; ko re mo ok!
; (define uk-coins (list 100 50 20 10 5 2 1 0.5))
; (display (cc 100 us-coins))(newline)
