(load "2.2.3.scm")

; zip-like
(define (accumulate-n op null-var seqs)
  (if (null? (car seqs)) ; shortest
    '()
    (cons 
      (accumulate op null-var (map car seqs))
      (accumulate-n op null-var (map cdr seqs))
    )
  )
)

; test
; (display (accumulate-n + 0 (list '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12))))(newline)
