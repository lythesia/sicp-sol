; rec-1
(define (repeated f n)
  (define (iter i)
    (if (= i 1) (lambda (x) (f x))
      (lambda (x) (f ((iter (- i 1)) x)))
    )
  )
  (iter n)
)

; rec-2
(define (repeated f n)
  (if (= n 1) f
    (lambda (x) (f ((repeated f (- n 1)) x))))
)

; iter
(define (repeated f n)
  (define (iter i rep)
    (if (= i 1) rep
      (iter (- i 1) (lambda (x) (f (rep x)))))
  )
  (iter n f)
)

(load "1.42.scm")
; fast-power like
; rec
(define (repeated-fast f n)
  (cond ((= n 1) f)
        ((even? n) 
          (let ((rep (repeated f (/ n 2))))
            (compose rep rep)
          )
        )
        (else
          (compose f (repeated f (- n 1)))
        )
    )
)

; iter consider 0
(define (identity x) x)
(define (repeated-fast f n)
  (define (iter nf i rep)
    (cond ((= i 0) rep) ; count down to 1 indicates one more nf not handled
          ((even? i) (iter (compose nf nf) (/ i 2) rep))
          (else (iter nf (- i 1) (compose rep nf)))
    )
  )
  (iter f n identity)
)

; not consider n=0
; (display ((repeated-fast 1+ 2) 1))(newline)
; (display ((repeated-fast (lambda (x) (* x x)) 2) 5))(newline)
