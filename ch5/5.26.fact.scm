(define (factorial n) ; push 3
  (define (iter p c)  ; push 3
    (if (> c n) p (iter (* p c) (+ c 1)))
  )                   ; pop 3
  (iter 1 1)
)                     ; pop 3

; try (factorail ..)
; in actual apply:
; max pushes in (iter 1 1):
; push continue
; first exp: if
; last exp: pop continue, goto eval-if
; push exp env continue (3)
; (> c n): goto eval-apply
; push continue env unev (6), lookup operator >
; pop unev env (4), push proc (5)
; push argl (6), not last arg: push env unev (8), lookup variable c (first arg done), goto accum-arg: pop env unev argl (5)
; push argl (6), last arg: lookup variable n (args done), accum-last-arg: pop argl proc (4), goto apply-primitive-procedure
; pop continue (3)
;; surely we should choose (iter (* p c) (+ c 1))
; pop continue env exp (0)
;; another round of apply (but this time is compound) 
; push continue env unev (3), look up iter
; pop unev env (1), push proc (2)
; push argl (3) first arg: push env unev (5), eval (* p c)
; push continue env unev (8), look up *
; pop unev env (6), push proc (7)
; push argl (8) first arg: push env unev (**10**), lookup variable p, accum-arg -> 7
; last-arg -> 6
; pop continue -> 5
; eval (+ c 1) --> 10 --> 5
; ...
