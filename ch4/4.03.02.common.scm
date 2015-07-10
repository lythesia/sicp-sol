(define (distinct? lst)
  (cond
    ((null? lst) #t)
    ((null? (cdr lst)) #t)
    ((member (car lst) (cdr lst)) #f)
    (else (distinct? (cdr lst)))
  )
)

(define (list-swap lst i j)
  (if (= i j)
    lst
    (let ((vi (list-ref lst i)) (vj (list-ref lst j)))
      (list-set! lst i vj)
      (list-set! lst j vi)
      lst
    )
  )
)

(define (!= x y) (not (= x y)))
