;; it's trivial why it's twiced.

;; one solution: sort it
(rule
  (lives-near-ordered ?person1 ?person2)
  (and
    (lives-near ?person1 ?person2)
    (lisp-value > ?person1 ?person2) ; here > is **special** implemented
  )
)
