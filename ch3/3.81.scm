(load "3.05.01.stream.scm")
(load "3.50.scm")
(load "3.05.02.is.scm")

(define (rand-update x)
  (let ((a 1103515245) (c 12345) (m 2147483648))
    (modulo (+ (* a x) c) m)
  )
)

(define (rand-stream random-init action-stream)
  (define (action x m)
    (cond
      ((eq? m 'generate) (rand-update x))
      (else m)
    )
  )
  (cons-stream
    random-init
    (stream-map action (rand-stream random-init action-stream) action-stream)
  )
)

; test
; (define s0 (cons-stream 'generate s0))
; (define rs0 (rand-stream 137 s0))
; (stream-head rs0 5)(newline)  ; 137 3062 1397 9182 1142 ..

; (define s1 (cons-stream 'generate (cons-stream 'generate (cons-stream 367 (cons-stream 'generate (cons-stream 137 s0))))))
; (define rs1 (rand-stream 137 s1))
; (stream-head rs1 10)(newline)  ; 137 3062 1397 367 652 137 3062 1397 9182 1142 ..
