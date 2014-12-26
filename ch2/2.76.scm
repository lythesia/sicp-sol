; case match
;===================
; explicit dispatch
;===================
; (define (some-generic-op arg)
;   (cond 
;     ((tag-x? arg) ..)
;     ((tag-y? arg) ..)
;     ; ..
;     (else (error ..))
;   )
; ) 


; table-base
; prefer new ops/datatypes
;===================
; data driven
;===================
; (put op type impl)
; (get op type) => impl
;
; (define (apply-generic op args)
;   (let* ((tag (type-tag args)) (proc (get op tag)))
;     (apply proc (content args))
;   )
; )


; OO-like
; prefer new datatypes
;===================
; message driven
;===================
; (define (some-form args)
;   (lambda (op)
;     (cond
;       ((match op some-func-impl) (impl args))
;       ; ..
;       (else (error ..))
;     )
;   )
; )
; (define (apply-generic op arg) (arg op))

