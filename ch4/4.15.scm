;; Halting problem

(define (run-forever) (run-forever))  ; infinitely call itself

; [Cantor's diagonal](http://en.wikipedia.org/wiki/Cantor%27s_diagonal_argument) 
; if p halts on self, then make it run-forever, else halt it (the diagonal of `halts?`!)
(define (try p)
  (if (halts? p p)
    (run-forever)
    'halted
  )
)

; for (try try)
; if (halts? try try) -> then (run-forever) => (try try) wil not halt, contract with (halts? try try)
; else -> 'halted, contract with (else)
