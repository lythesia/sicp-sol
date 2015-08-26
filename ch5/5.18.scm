;; modify in 5.02.01.mac.scm
(define (make-register name)
  (let ((contents '*unassigned*) (watch #f))
    (define (dispatch message)
      (cond
        ((eq? message 'trace-on) (set! watch #t))
        ((eq? message 'trace-off) (set! watch #f))
        ((eq? message 'get) contents)
        ((eq? message 'set)
         (lambda (value)
           (if watch
             (begin
               (display name)(display ": ")(display contents)(display " -> ")(display value)(newline)
             )
           )
           (set! contents value)
         )
        )
        (else (error "Unknown request -- REGISTER" message))
      )
    )
    dispatch
  )
)

;; or in inherit style
(define make-register-old make-register)
(define (make-register name)
  (let ((watch #f) (register-base (make-register-old)))
    (define (dispatch message)
      ; override
      (cond
        ((eq? message 'set)
         (lambda (value)
           (if watch
             (begin
               ; i'd like format..
               (display name)(display ": ")(display (register-base 'get))(display " -> ")(display value)(newline)
             )
           )
           ((register-base 'set) value)
         )
        )
      )
      ; added
      ((eq? message 'trace-on) (set! watch #t))
      ((eq? message 'trace-off) (set! watch #f))
      (else (register-base message))
    )
    dispatch
  )
)
