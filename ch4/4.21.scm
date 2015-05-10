;; a)
; ((lambda (n)
;   ((lambda (fact)
;     (fact fact n)
;    )
;    (lambda (ft k) ; as arg -> fact
;      (if (= k 1)
;        1
;        (* k (ft ft (- k 1)))
;      )
;    )
;   ) ; as a func
;  )
;  10)

;; call with n = 10:
; ((lambda (fact) ; func
;   (fact fact 10)
;  )
;  (lambda (ft k) ; call arg -> fact
;     ;..
;  )
; )

;; thus
; (lambda-ft-k lambda-ft-k 10) ; lambda-ft-k covers (lambda (ft k) ..)

;; so whole process as:
; (fact fact 10)
; (* 10 (ft ft 9))
;       `-> (* 9 (ft ft 8))
;           `-> (* 8 (ft ft 7))
;               ..
;                `-> (* 2 (ft ft 1))
;                         1

(display
  ((lambda (n)
    (
     (lambda (fib)
       (fib fib n)
     )
     (lambda (f k)
       (if (<= k 2)
         1
         (+ (f f (- k 1)) (f f (- k 2)))
       )
     )
    )
   )
   10
  )
)(newline)  ; 55

;; b)
(define (f x)
  (
    (lambda (even? odd?)
      (even? even? odd? x)
    )
    (lambda (ev? od? n) ; this is `even?`
      (if (= n 0) #t (od? ev? od? (- n 1)))
    ) 
    (lambda (ev? od? n) ; this is `odd?`
      (if (= n 0) #f (ev? ev? od? (- n 1)))
    )
  )
)
;; odd? should call even?, so in the real odd? (ev? <?> <?> (- n 1)) should be in form (even? even? odd? n-1),
;; then we know ev? bind to true even? od? bind to true odd?
;;
;; even? should call odd? on (n-1), so od?(in real even?) must be the true odd?, as we see initially the true odd?
;; is bind to od?; for same, odd? should call even? on (n-1), so ev?(in real odd?) must be true even?, so we step
;; back to real even?: (od? <?> <?> (- n 1)), where 1st arg is mapped to ev?, and it should be true even?, so this
;; arg should be true even?, as we initially bind true even? to ev?, so 1st arg is ev? and so 2nd arg is true odd?

; test
(display (f 3))(newline)
(display (f 4))(newline)
