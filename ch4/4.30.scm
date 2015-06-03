;; a)
(define (for-each proc items)
  (if (null? items)
    'done
    (begin
      (proc (car items))
      (for-each proc (cdr items))
    )
  )
)
; test
; 4.02.02.delay.scm < $0
(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
(exit)
;; will give <NL>56<NL>321<NL>88
;; since `display` is primitive, eval on `x` is forced

;; b)
;           original  modified
; (p1 1) -> (1 . 2)   (1 . 2)
; (p2 1) -> 1         (1 . 2)

;; c)
;; Can force eval be wrong? I think it's trivial.

;; d)
;; I prefer text ver. Coder should have control on the procedure whether to be lazied or not, or
;; say they can force it once in need.
