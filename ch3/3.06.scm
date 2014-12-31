(define random-init 9527)
(define rand
  (let ((x random-init))
    (lambda (mode)
      (cond
        ((eq? 'generate mode) (set! x (rand-update x)) x)
        ((eq? 'reset mode) (lambda (new-x) (set! x new-x)))
        (else (error "Unknown mode -- RAND" mode))
      )
    )
  )
)
; no more than 16
(define (rand-update x)
  (remainder (+ (* x 8) 5) 17)
)

; test
; (display (rand 'generate))(newline)
; (display (rand 'generate))(newline)
; (display (rand 'generate))(newline)
; ((rand 'reset) random-init)
; (display (rand 'generate))(newline)
; (display (rand 'generate))(newline)
; (display (rand 'generate))(newline)
