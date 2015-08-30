(include "5.05.compile.scm")

(define compiled-instruction-sequence
  (compile
    '(define (factorial n)
       (if (= n 1)
         1
         (* (factorial (- n 1)) n)))
    'val
    'next
  )
)

(map (lambda (x) (display x)(newline)) (statements compiled-instruction-sequence))
