(load "2.65.scm")

(define (lookup given-key set)
  (if (null? set) #f
    (let* ((this (entry set)) (this-key (key this)))
      (cond
        ((= this-key given-key) this)
        ((< this-key given-key) (lookup given-key (right set)))
        (else (lookup given-key (left set)))
      )
    )
  )
)

; test
; (define (key record) (car record))
;                   (7 "John")
;                   /        \
;                  /          \
;           (3 "Mary")       (19 "Tom")
;           /     \
; (1 "Peter")    (5 "Jack")
; (define s '( (7 "John") ( (3 "Mary") ( (1 "Peter") () () ) ( (5 "Jack") () () )) ( (19 "Tom")  () () ) ))
; (display (lookup '5 s))(newline)
; (display (lookup '0 s))(newline)
