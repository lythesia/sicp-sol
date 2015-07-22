(controller
  test-counter
  (test (op >) (reg counter) (reg n))
  (branch (label factorial-done))
  (assign product (op *) (reg product) (reg counter))
  (assgin counter (op +) (reg counter) (const 1))
  (goto (label test-counter))
  factorial-done
)
