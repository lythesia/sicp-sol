(define (count-pairs x)
  (if (not (pair? x)) 0
    (+ (count-pairs (car x)) (count-pairs (cdr x)) 1)
  )
)

; test
; +---+---+    +---+---+    +---+---+
; | x | +----->| x | +----->| x | / |
; +---+---+    +---+---+    +---+---+
(define t1 (list 1 2 3))
(display (count-pairs t1))(newline)

; +---+---+
; | + | +--------+
; +-|-+---+      |
;   |            |
;   v            v
; +---+---+    +---+---+
; | x | +----->| x | / |
; +---+---+    +---+---+
(define pair (list 1 2))
(define t2 (cons pair (cdr pair)))
(display (count-pairs t2))(newline)

; +---+---+ 
; | + | + |
; +-|-+-+-+
;   |   |
;   |---+ 
;   v     
; +---+---+
; | + | + |
; +-|-+-+-+ 
;   |   |
;   |---+ 
;   v     
; +---+---+  
; | x | / |
; +---+---+
(define p-sub-sub (cons 1 '()))
(define p-sub (cons p-sub-sub p-sub-sub))
(define t3 (cons p-sub p-sub))
(display (count-pairs t3))(newline)
