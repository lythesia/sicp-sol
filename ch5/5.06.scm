...
afterfib-n-1
  (restore n)
  (restore continue)  ; ---------------------.
  (assign n (op -) (reg n) (const 2)) ;       this pair can be removed, since no effect
  (save continue)     ; ---------------------'
  (assign continue (label afterfib-n-2))
...
