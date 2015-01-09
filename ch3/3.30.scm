(load "3.03.04.simula.scm")
(load "3.03.04.fa.scm")

(define (ripple-carry-adder A B Cin S Cout)
  (define (iter A B cin S cout)
    (if (null? A)
      ; (add-action! cin (cons "assign-cout" (lambda() (set-signal! Cout (get-signal cin)))))
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

(define a1 (make-wire))
(define b1 (make-wire))
(define s1 (make-wire))

(define a2 (make-wire))
(define b2 (make-wire))
(define s2 (make-wire))

(define cin (make-wire)) ; 0
(define cout (make-wire))

(set-signal! cin 0)

; predifine is ok
; (ripple-carry-adder (list a0 ) (list b0 ) cin (list s0 ) cout)
; (ripple-carry-adder (list a0 a1 ) (list b0 b1 ) cin (list s0 s1 ) cout)
(ripple-carry-adder (list a0 a1 a2) (list b0 b1 b2) cin (list s0 s1 s2) cout)
; (print-agenda the-agenda)
; (propagate)
; (display (get-signal s0))
; (display ":")(display (get-signal cout))(newline)

(set-signal! a0 1)
(set-signal! a1 0)
(set-signal! a2 1)

(set-signal! b0 1)
(set-signal! b1 1)
(set-signal! b2 0)
; (newline)
; (print-agenda the-agenda) ; will be less actions(only related to a0 b0)

; later define is ok
; (ripple-carry-adder (list a0 a1 a2) (list b0 b1 b2) cin (list s0 s1 s2) cout)
; (ripple-carry-adder (list a0 a1 ) (list b0 b1 ) cin (list s0 s1 ) cout)
; (ripple-carry-adder (list a0 ) (list b0 ) cin (list s0 ) cout)
; (print-agenda the-agenda)

; 1 + 1 = 0:1

; 01 + 11 = 00:1

; 101 + 011 = 000 : 1

(propagate)
(display "result: ")
(display (get-signal s2))
(display (get-signal s1))
(display (get-signal s0))
(display ":")(display (get-signal cout))(newline)


; summary
; a procedure that construct complex gate logic, it completes following things:
;   1. each wire in global env is bound actions: these **ACTIONS** adds set-out procs to global agenda, the out-val to be set is already computed in currently-evaluated env
;      **NOTE** in action list of each wire, so order does not matters
;   2. when each `add-action!` invoked, corrrespond set-out proc is added to the agenda
;
; set-signal!
;   when set signal of input-wire (the input-wire can be internals the logic circuit), then **ACTIONS** bound to that wire is re-called one-by-one, so that append to agenda according to current time, newly added **MUST** be append last in corrrespond segment
;   **NOTE** only signal changes trigger the re-bind procedure
;
; propagate
;   1. call first-proc in agenda one-by-one
;   2. when proc is to set-signal! then the output-wire's actions rebind, which append to agenda (plus current time, so that the time order is preserved)
