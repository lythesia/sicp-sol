;; a) count-leaves
(
  (assign continue (label done))
  (assign val (const 0))
test-loop
  (test (op null?) (reg tree))
  (branch (label null-tree))
  (test (op pair?) (reg tree))
  (branch (label left-tree))
  (assign val (const 1))
  (goto (reg continue))
left-tree
  (save tree)
  (save continue)
  (assign tree (op car) (reg tree))
  (assign continue (label right-tree))
  (goto (label test-loop))
right-tree
  (restore continue)
  (restore tree)
  (save continue)
  (save val)
  (assign tree (op cdr) (reg tree))
  (assign continue (label after-tree))
  (goto (label test-loop))
after-tree
  (assign var (reg val))  ; right val
  (restore val)           ; left val
  (restore continue)
  (assign val (op +) (reg val) (reg var))
  (goto (reg continue))
null-tree
  (assign val (const 0))
  (goto (reg continue))
done
)

;; b)
(
  (assign conitnue (label done))
  (assign val (const 0))
  (save continue)
  (assign continue (label outer-right-tree))
test-loop
  (test (op null?) (reg tree))
  (branch (label null-tree))
  (test (op pair?) (reg tree))
  (branch (label left-tree))
  (assign val (op +) (reg val) (const 1))
  (restore continue)
  (goto (reg continue))
inner-left-tree
  (save tree)
  (save continue)
  (assign tree (op car) (reg tree))
  (goto (label test-loop))
outer-right-tree
  (restore tree)
  (assign tree (op cdr) (reg tree))
  (goto (label test-loop))
null-tree
  (restore conitnue)
  (goto (reg continue))
done
)
