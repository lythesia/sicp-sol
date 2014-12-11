(define l1 (list 1 3 (list 5 7) 9))
; (display (car (cdr (car (cdr (cdr l1))))))(newline)

(define l2 (list (list 7)))
; (display (car (car l2)))(newline)

(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; (display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3)))))))))))))(newline)

; short ver
; (display (cadadr (cdr l1)))(newline)
; (display (caar l2))(newline)
; (display (cadadr (cadadr (cadadr l3))))(newline)
