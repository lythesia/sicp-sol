;; as concept of spontaneous effect in scope, Eva should be right; but as the implemention int textbook, Alyssa is
;; right, see converting:
;;
;; (let ((a 1))
;;   (define (f x)
;;     (let ((b '*unassigned*) (a '*unassigned*))
;;       (set! b (+ a x)) ; <- at this point, a is '*unassigned*
;        (set! a 5)
;        (+ a b)
;;     )
;;   )
;;   (f 10)
;; )
;;
;;
;; one possible solution using lazy eval, compute only in-need, so in every place variable is used, force it:
(let ((a 1))
  (define (f x)
    (let ((b '*unassigned*) (a '*unassigned*))
      (set! b (delay (+ (force a) x)))
      (set! a (delay 5))
      (+ (force a) (force b)) ; should be 20
    )
  )
  (display (f 10))(newline)
)
