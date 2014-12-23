(load "2.2.scm")

; top-down

; two top interface
(define (peri-rec r)
  (let ((xl (xlen-rec r)) (yl (ylen-rec r)))
    (* 2 (+ xl yl))
  )
)
(define (area-rec r)
  (let ((xl (xlen-rec r)) (yl (ylen-rec r)))
    (* xl yl)
  )
)

; two top interface use these
; which depends how we construct the rectangle
(define (xlen-rec r) (car (cdr r)))
(define (ylen-rec r) (cdr (cdr r)))

; construct
(define (make-rec ul x y)
  (cons ul (cons x y))
)

(define (up-left-rec r) (car r))
(define (up-right-rec r) 
  (let ((ul (car r)) (xl (xlen-rec r)))
    (make-point (+ (x-point ul) xl) (y-point ul))
  )
)
(define (bot-left-rec r)
  (let ((ul (car r)) (yl (ylen-rec r)))
    (make-point (x-point ul) (+ (y-point ul) yl))
  )
)
(define (bot-right-rec r)
  (let ((ul (car r)) (xl (xlen-rec r)) (yl (ylen-rec r)))
    (make-point (+ (x-point ul) xl) (+ (y-point ul) yl))
  )
)

; test
; (define upleft (make-point 1 4))
; (define rectangle (make-rec upleft 3 2))

; (display (xlen-rec rectangle))(newline)
; (display (ylen-rec rectangle))(newline)
; (display (peri-rec rectangle))(newline)
; (display (area-rec rectangle))(newline)
