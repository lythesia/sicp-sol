;; e.g: i = 4, j = 5, there's no k that satifies k^2 = 4^2 + 5^2, but with
;; an-integer-starting-from, it will search infinitely, so it never ends.

;; we can first fix k, which can be search from low to infinite, then fix j, then i, both given an upper-bound
(define (phythagoras-triple-from low)
  (let ((k (an-integer-starting-from low)))
    (let ((j (an-integer-between (low k))))
      (let ((i (an-integer-between (low j))))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)
      )
    )
  )
)
