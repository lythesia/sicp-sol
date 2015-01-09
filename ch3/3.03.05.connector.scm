; connector
(define (make-connector)
  (let ((value #f) (info #f) (constraints '()))
    (define (set-value! new-val setter)
      (cond
        ; no value, can set
        ((not (has-value? me)) (set! value new-val) (set! info setter) (for-each-except setter inform-about-value constraints))
        ; has value, cannot set a diff value
        ((not (= value new-val)) (error "Contradiction set-value" (list value new-val)))
        (else 'ignored)
      )
    )
    (define (forget-value! retractor)
      ; who set value are allowed to forget-value
      (if (eq? retractor info)
        (begin
          (set! info #f)
          (for-each-except retractor inform-about-no-value constraints)
        )
        ; constant fall to this
        'ignored
        ; (begin (display retractor)(display" :> ")(display info)(display " | val: ")(display value)(display " ignore forget!\n"))
      )
    )
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints (cons new-constraint constraints))
      )
      ; inform immediately if has value
      (if (has-value? me)
        (inform-about-value new-constraint)
      )
      'done
    )

    (define (me request)
      (cond
        ((eq? request 'has-value?) (if info #t #f))
        ((eq? request 'get-value) value)
        ((eq? request 'set-value!) set-value!)
        ((eq? request 'forget-value!) forget-value!)
        ((eq? request 'connect) connect)
        (else (error "Unknown request -- CONNECTOR" request))
      )
    )
    me
  )
)

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'get-value))
(define (set-value! connector new-val info) ((connector 'set-value!) new-val info))
(define (forget-value! connector retractor) ((connector 'forget-value!) retractor))
(define (connect connector new-constraint) ((connector 'connect) new-constraint))
