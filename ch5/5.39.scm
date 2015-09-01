(define (make-lexical-address addr-frame addr-offset) (list addr-frame addr-offset))
(define addr-frame car)
(define addr-offset cadr)

(define (lexical-address-lookup address env)
  ; assert within range
  (let* ((frame (list-ref env (addr-frame address))) (val (list-ref (frame-values frame) (addr-offset address))))
    (if (eq? val '*unassigned*)
      (error "Unassigned vairable -- LEXICAL-ADDRESS-LOOKUP" address)
      val
    )
  )
)
(define (lexical-address-set! address env val)
  (let ((frame (list-ref env (addr-frame address))))
    (list-set! (frame-values frame) (addr-offset address) val)
  )
)
