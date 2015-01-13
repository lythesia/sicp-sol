(load "3.05.01.stream.scm")
(load "3.50.scm") ; generic map

(define (show x)
  (display-line x)
  x
)

(define x (stream-map show (stream-enum-interval 0 10))) 
; (stream-map show (0 . delay([1..10])))
; => 0
; (0 . delay(stream-map show (stream-cdr (0 . delay([1..10])))))

(stream-ref x 5)
; note x is modified under guile's delay/force
; (0 . (delay
;       {1 . (delay
;             {2 . (delay
;                   {3 . (delay
;                         {4 . (delay
;                               {5 . (delay
;                                     (stream-map show (s-cdr {5 {delay [6 10]}})))})})})})
(display x)

(display "\n====\n")

(stream-ref x 7)
