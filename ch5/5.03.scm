; simple
(controller
  (assign x (op read))
  (assign guess (const 1.0))
test-guess
  (test (op good-enough?) (reg guess))
  (branch (label sqrt-done))
  (assign guess (op improve) (reg guess))
  (goto (label test-guess))
sqrt-done
)

; full
(controller
  ; init
  (assign x (op read))
  (assign guess (const 1.0))

test-guess
  ; good-enough?
  (assign t (op square) (reg guess))
  (assign t (op -) (reg t) (reg x))
  (assign t (op abs) (reg t))
  (test (op <) (reg t) (const 0.001))
  (branch (label sqrt-done))

  ; improve
  (assign t (op /) (reg x) (reg guess))
  (assign guess (op average) (reg guess) (reg t)) ; average assumed

  (goto (label test-guess))
sqrt-done
)
