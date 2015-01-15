(load "3.05.01.stream.scm")
(load "3.50.scm")
(load "3.54.scm") ; for mul-streams
(load "3.05.02.is.scm")

(define (div-streams s1 s2)
  (stream-map / s1 s2)
)

; a)
(define (integrate-series sa) (mul-streams (div-streams ones integers) sa))

; b)
; (e^x)' -> e^x
; s(e^x) -> s(integrate e^x)
(define exp-series (cons-stream 1 (integrate-series exp-series)))
; (display-stream-to exp-series 5)(newline)

(define neg-ones (cons-stream -1 neg-ones))
(define cosine-series (cons-stream 1 (mul-streams neg-ones (integrate-series sine-series))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))
; (display-stream-to cosine-series 5)(newline)
; (display-stream-to sine-series 5)(newline)
