; as dispatch
(define (make-wire)
  ; local state
  (let ((signal-val 0) (action-procs '()))
    (define (set-signal! new-val)
      (if (not (= signal-val new-val))
        (begin (set! signal-val new-val) (call-each action-procs)) ; call-each rebind action to each wire (that cause add action to agenda)
        'done
      )
    )

    (define (accept-action-proc! proc)
      ; prepend ok
      ; (set! action-procs (cons proc action-procs))
      ; append ok
      (set! action-procs (append action-procs (list proc)))
      ; call
      ; (display "bind action to wire: ")(display (car proc))(newline)
      ; ((cdr proc))
      (proc)
    )

    (define (dispatch m)
      (cond
        ((eq? m 'get-signal) signal-val)
        ((eq? m 'get-actions) action-procs)
        ((eq? m 'set-signal!) set-signal!)
        ((eq? m 'add-action!) accept-action-proc!)
        (else (error "Unknown operation -- WIRE" m))
      )
    )
    dispatch
  )
)

(define (call-each procs)
  (if (null? procs)
    'done
    (begin
      ; ((cdar procs))
      ((car procs))
      (call-each (cdr procs))
    )
  )
)

; export
(define (get-signal wire) (wire 'get-signal))
(define (get-actions wire) (wire 'get-actions))
(define (set-signal! wire new-val) ((wire 'set-signal!) new-val))
(define (add-action! wire action-proc) ((wire 'add-action!) action-proc))
