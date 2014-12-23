(load "util.scm")

; church encoding
; http://en.wikipedia.org/wiki/Church_encoding#Computation_with_Church_numerals

(define (zero)
  (lambda (f) (lambda (x) x))
)

;; one def
;; f not mean anything
(define (one f)
  (lambda (f) (lambda (x) (f x)))
)

(define (two f)
  (lambda (f) (lambda (x) (f (f x))))
)

; similar with add-1
; m and n are concret number derived from one and two
(define (add m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x))
    )
  )
)

(display (((zero) 1+) 0))(newline)
(define onec (one inc)) ; not inc is ok
(define twoc (two inc)) ; not inc is ok
(display (((one 1+) 1+) 0))(newline)
(display (((two 1+) 1+) 0))(newline)
(define threec (add twoc onec))
(display ((threec 1+) 0))(newline)
;;

;; another def
;; standard def
(define one
  (lambda (f) (lambda (x) (f x)))
)

(define two
  (lambda (f) (lambda (x) (f (f x))))
)

; similar with add-1
(define (add m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x))
    )
  )
)

(display (((zero) 1+) 0))(newline)
(display ((one 1+) 0))(newline)
(display ((two 1+) 0))(newline)
(define three (add one two))
(display ((three 1+) 0))(newline)
