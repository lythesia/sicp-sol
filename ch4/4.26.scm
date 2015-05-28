(define dont-run 1)
(include "4.01.no-aly.scm")

;;deriv from if
; grammar
;  :unless cond except [usual]
(define (unless? exp)
  (tagged-list? exp 'unless)
)
(define (unless-cond exp)
  (cadr exp)
)
(define (unless-except exp)
  (caddr exp)
)
(define (unless-usual exp)
  (if (null? (cdddr exp))
    'false
    (cadddr exp)
  )
)
; a) `not` approach
; note this error!
;
; (display
;   (unless->if '(unless (= b 0) (display (/ a b)) (display "b = 0!")))
; )
; gives:
; (if (not = b 0) (display (/ a b)) (display b = 0!)) <- this lack a () wrapper for condition!
; (define (make-not exp)
;   (cons 'not (list exp))
; )
; (define (unless->if exp)
;   (make-if (make-not (unless-cond exp)) (unless-except exp) (unless-usual exp))
; )

; b) switch cons and alter approach
(define (unless->if exp)
  (make-if (unless-cond exp) (unless-usual exp) (unless-except exp))
)

(define old-eval eval)
(define (eval exp env)
  ; put this at head(ok?)
  (cond
    ((unless? exp) (eval (unless->if exp) env))
    (else (old-eval exp env))
  )
)
(driver-loop)


;; Alyssa's point:
;; if unless is implemented as a procedure, so it can be used as arguments in high-class procedures like: map
