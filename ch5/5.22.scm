;; a)
(
  ; x, y
  (assign continue (label done))
  (save continue)
append
  (test (op null?) (reg x))
  (branch (label x-null))
  (assign temp (op car) (reg x))
  (save temp)
  (assign continue (label x-after))
  (save continue)
  (assign x (op cdr) (reg x))
  (goto (label append))
x-after
  (restore x)
  (assign val (op cons) (reg x) (reg val))
  (restore continue)
  (goto (reg continue))
x-null
  (assign val (reg y))
  (restore continue)
  (goto (reg continue))
done
)

;; b)
(
  ; x y
  (test (op null?) (reg x))
  (branch (label null))
  (assign curr (reg x))
  (goto (label last-pair?))
append!
  (perform (op set-cdr!) (reg curr) (reg y))
  (goto (label done))
last-pair?
  (assign next (op cdr) (reg curr))
  (test (op null?) (reg next))
  (branch (label append!))
  (assign curr (op cdr) (reg curr))
  (goto (label last-pair?))
null
  (assign x (reg y))
done
)
