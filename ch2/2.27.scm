(define (deep-reverse l)
  (define (iter rest result)
    (if
      (null? rest) result
      (let ((curr (car rest)))
        ; (if (list? curr) ; (list? '()) -> #t
        (if (pair? curr) ; (pair? '()) -> #f
          (iter (cdr rest) (cons (iter curr '()) result))
          (iter (cdr rest) (cons curr result))
        )
      )
    )
  )
  (iter l '())
)

; graceful!
(define (deep-reverse l)
  (if (pair? l)
    (reverse (map deep-reverse l))
    l
  )
)

; test
; (define x (list (list 1 2 (list 2.5)) '() (list 3 4)))
; (display x)(newline)
; (display (reverse x))(newline)
; (display (deep-reverse x))(newline)
