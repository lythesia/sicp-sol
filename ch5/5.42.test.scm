(include "5.40.compile.scm")  ; compiler with compile-time-env
(include "5.40.scm")  ; compile-lambda-body
(include "5.42.scm")  ; compile-variable, compile-assignment

(define compiled-instruction-sequence
  (compile
    '(((lambda (x y)
        (lambda (a b c d e)
          ((lambda (y z) (* x y z))
           (* a b x)
           (+ c d x))))
      3 4)
     1 2 3 4 5)
    'val
    'next
    the-empty-compile-environment
  )
)

(map (lambda (x) (display x)(newline)) (statements compiled-instruction-sequence))
