(use-modules (ice-9 time))

(define dont-run-lazy 1)
(include "4.02.02.delay.scm")

;; a)
;; 1) no-memo
;; override force-it
;; comment out to use memo version
; (define (force-it obj)
;   (if (thunk? obj)
;     (actual-value (thunk-exp obj) (thunk-env obj))
;     obj
;   )
; )
;; result:
;; clock utime stime cutime cstime gctime
;; 10.71 10.86  0.12   0.00   0.00   0.41

;; 2) memo
;; result:
;; clock utime stime cutime cstime gctime
;;  0.36  0.37  0.00   0.00   0.00   0.02
(time (driver-loop))

;; b)
;; (define (square x) (* x x))
;; >> (square (id 10))
;; 100
;; >> count
;; 1 if memo
;; 2 if no-memo
;;
;; (* x x)
;;    | +---------------------------------+
;;    v                                   |
;; x <- '(id 10)                          |
;; |      `-> force-it eval '(id 10)      |
;; |          : count <- 1                |
;; v          :[return] 10                |
;; x <- list('evaluated-thunk 10 '()) <---+
