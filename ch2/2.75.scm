(define (make-from-mag-ang r a)
  (lambda (op)
    (cond
      ((eq? op 'real-part) (* r (cos a)))
      ((eq? op 'imag-part) (* r (sin a)))
      ((eq? op 'magnitude) r)
      ((eq? op 'angle) a)
      (else (error "unknown operation -- MAKE-FROM-MAG-ANG" op))
    )
  )
)
(define (apply-generic op arg) (arg op))

; test
; (define z (make-from-mag-ang 4 0))
; (display (apply-generic 'real-part z))(newline)
; (display (apply-generic 'imag-part z))(newline)
; (display (apply-generic 'magnitude z))(newline)
; (display (apply-generic 'angle z))(newline)
