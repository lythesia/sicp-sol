; (append-to-form x y z): x + y -> z
(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z)) ; pattern
  (append-to-form ?v ?y ?z) ; apply
)

; meteorgan
; (rule (reverse () ()))
; (rule (reverse ?x ?y)
;   (and
;     (append-to-form (?first) ?rest ?x)
;     (append-to-form ?rev-rest (?first) ?y)
;     (reverse ?rest ?rev-rest)
;   )
; )

; mine(is this ok?)
(rule (reverse () ()))
(rule (reverse (?u . ?x) ?z)
  (and
    (append-to-form ?y (?u) ?z)
    (reverse ?x ?y)
  )
)

(reverse (1 2 3) ?x)
; no, in meteorgan's ver will call (append-to-form ?rev-rest (?first) ?y) inf, despite (?first) = (1), while
; ?rev-rest and ?y both unbound, will produce inf append pairs.
; no, neither in mine, similarly.

(reverse ?x (1 2 3))
; i think no, in meteorgan's ver, it will stun in 1st call to append-to-form
; ok in mine (3 2 1).
