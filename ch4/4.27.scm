; 4.02.02.delay.scm < $0
(define count 0)
(define (id x)
  (set! count (+ count 1))
  x
)
(define w (id (id 10)))

; >>
count ; 1 first assignment (define w ..) is strict:
;;        eval '(id (id 10)) env
;;        `-> apply (actual 'id env) '(id 10) env
;;            apply <proc> '(id 10) env
;;            `-> eval-sequence .. : where x <- delayed '(id 10) env
;;                count <- 1
;;                x <- (list 'thunk '(id 10) env)
;;                w <- x (bind by assignment)
; >>
w     ; 10
; >>
count ; 2  <- since forced
(exit)
