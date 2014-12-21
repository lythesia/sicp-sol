(load "2.2.3.scm")

(define empty-board '())

; pos = [y_{k}, y_{k-1}, .. y1]
(define (safe? k pos)
  (let ((new-row (car pos)) (new-col k))
    (define (iter i rest)
      (if (= i 0) #t
        (let ((cur-row (car rest)) (cur-col i))
          (if (or (= cur-row new-row) (= (abs (- new-row cur-row)) (abs (- new-col cur-col)))) #f
            (iter (1- i) (cdr rest))
          )
        )
      )
    )
    (iter (1- k) (cdr pos))
  )
)

; [new, rest]
(define (adjoin-position new-row k rest)
  (cons new-row rest)
)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row) (adjoin-position new-row k rest-of-queens)) (enumerate-interval 1 board-size))
          )
          (queen-cols (1- k))
        )
      )
    )
  )
  (queen-cols board-size)
)

; test
; (display (safe? 4 (list 5 8 2 4)))(newline)
; (for-each (lambda (p) (begin (display p)(newline))) (queens 8))
