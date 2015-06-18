;; assum we have amb and require
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high))
)

; what if ? check it until me have a true amb!
; (amb low high (an-integer-between (+ low 1) (- high 1)))
