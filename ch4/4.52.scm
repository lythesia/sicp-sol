(define dont-run-amb 1)
(include "4.03.03.amb.scm")

(define old-analyze analyze)

(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define (analyze exp)
  (if (if-fail? exp)
    (analyze-if-fail exp)
    (old-analyze exp)
  )
)

(define (analyze-if-fail exp)
  (let ((first (analyze (cadr exp))) (second (analyze (caddr exp))))
    (lambda (env succeed fail)
      (first
        env
        (lambda (value fail2)
          (succeed value fail2)
        )
        (lambda ()
          (second env succeed fail) ; <- if 1st try fail, continue try(via succeed) in the fail branch
        )
      )
    )
  )
)

(driver-loop)
