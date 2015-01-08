(load "3.03.04.simula.scm")
(load "3.03.04.fa.scm")

(define (ripple-carry-adder A B Cin S Cout)
  (define (iter A B cin S cout)
    (if (null? A)
      (add-action! cin (lambda() (set-signal! Cout (get-signal cin))))
      (let ((a (car A)) (b (car B)) (s (car S)))
        (full-adder a b cin s cout)
        (iter (cdr A) (cdr B) cout (cdr S) (make-wire))
      )
    )
  )
  (iter A B Cin S (make-wire))
)

; test
(define a0 (make-wire))
(define b0 (make-wire))
(define s0 (make-wire))

; (define a1 (make-wire))
; (define b1 (make-wire))
; (define s1 (make-wire))

; (define a2 (make-wire))
; (define b2 (make-wire))
; (define s2 (make-wire))

(define cin (make-wire))
(define cout (make-wire))

; (set-signal! cin 0)

(set-signal! a0 1)
; (set-signal! a1 0)
; (set-signal! a2 1)

(set-signal! b0 1)
; (set-signal! b1 1)
; (set-signal! b2 0)

; must define after (set-signal!)
; (ripple-carry-adder (list a0 a1 a2) (list b0 b1 b2) cin (list s0 s1 s2) cout)
; (ripple-carry-adder (list a0 a1 ) (list b0 b1 ) cin (list s0 s1 ) cout)
(ripple-carry-adder (list a0 ) (list b0 ) cin (list s0 ) cout)
(print-agenda the-agenda)

; 1 + 1 = 0:1

; 01 + 11 = 00:1

; 101 + 011 = 000 : 1

; (propagate)
; (display (get-signal s2))
; (display (get-signal s1))
; (display (get-signal s0))
; (display ":")(display (get-signal cout))(newline)
