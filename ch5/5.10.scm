;; implement ++
; (inc reg)
(define (make-inc inst machine pc)
  (let ((target (get-register machine (inc-reg-name inst))))
    (lambda () (set-contents! target (+ (get-contents target) 1)))
  )
)
(define inc-reg-name cadr)
