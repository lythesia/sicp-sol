; This is NOT correct, because `pairs` is not a `cons-stream` struct, 2nd arg of
; `interleave` will call `pairs`, and then `interleave` again, infinitely.
; call-by-value works here (no lazy)
;
; (define (pairs s t)
;   (interleave
;     (stream-map (lambda (x) (list (stream-car s) x)) t)
;     (pairs (stream-cdr s) (stream-cdr t))
;   )
; )
