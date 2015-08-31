(include "5.05.compile.scm")

(define compiled-instruction-sequence
  (compile
    `(define (factorial n)
      (define (iter product counter)
        (if (> counter n)
          product
          (iter (* product counter) (+ counter 1))
        )
      )
      (iter 1 1)
    )
    'val
    'next
  )
)

(map (lambda (x) (display x)(newline)) (statements compiled-instruction-sequence))

;; this version makes (iter (* ..) (+ ..)) tail recursion, since we dont need modify continue(and save) to comeback, and also process for (* ..) and (+ ..) are constant pushes, too.
;; while for (* n (factorial ..)), we need to comeback after call inner factorial, so save continue is essential, btw, me think others need, too: proc for save *'s, and argl for save different (k (factorial(k) ..)).
