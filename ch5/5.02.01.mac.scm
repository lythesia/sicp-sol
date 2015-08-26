;; machine
(define (make-machine register-names ops controller-text) ; text = code
  (let ((machine (make-new-machine)))
    (for-each
      (lambda (register-name) ((machine 'allocate-register) register-name))
      register-names
    )
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence) (assemble controller-text machine))
    machine
  )
)
(define (make-new-machine)
  (let 
    ((pc (make-register 'pc))
     (flag (make-register 'flag))
     (stack (make-stack))
     (the-instruction-sequence '())
    )
    (let
      ((the-ops (list (list 'initialize-stack (lambda () (stack 'initialize)))))
       (register-table (list (list 'pc pc) (list 'flag flag)))
      )
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "register redefined:" name)
          (set! register-table (cons (list name (make-register name)) register-table))
        )
      )
      (define (lookup-register name)
        (let ((val (assoc name register-table))) ; entry
          (if val
            (cadr val) ; register object
            (error "Unknown register:" name)
          )
        )
      )
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts) ; why null?
            'done
            (begin
              ((instruction-execution-proc (car insts))) ; will consume car
              (execute)
            )
          )
        )
      )
      (define (dispatch message)
        (cond
          ((eq? message 'start) (set-contents! pc the-instruction-sequence) (execute))
          ((eq? message 'install-instruction-sequence) (lambda (seq) (set! the-instruction-sequence seq)))
          ((eq? message 'allocate-register) allocate-register)
          ((eq? message 'get-register) lookup-register)
          ((eq? message 'install-operations) (lambda (ops) (set! the-ops (append the-ops ops))))
          ((eq? message 'stack) stack)
          ((eq? message 'operations) the-ops)
          (else (error "Unknown request -- MACHINE" message))
        )
      )
      dispatch
    )
  )
)
(define (start machine)
  (machine 'start)
)
(define (get-register machine register-name)
  ((machine 'get-register) register-name)
)
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name))
)
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
)

;; register (why need `name' ?)
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond
        ((eq? message 'get) contents)
        ((eq? message 'set) (lambda (value) (set! contents value)))
        (else (error "Unknown request -- REGISTER" message))
      )
    )
    dispatch
  )
)
(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))

;; stack
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s))
    )

    (define (pop)
      (if (null? s)
        (error "Empty stack -- POP")
        (let ((top (car s)))
          (set! s (cdr s))
          top
        )
      )
    )

    (define (initialize)
      (set! s '())
      'done
    )

    (define (dispatch message)
      (cond
        ((eq? message 'push) push)
        ((eq? message 'pop) (pop))
        ((eq? message 'initialize) (initialize))
        (else (error "Unknown request -- STAKC" message))
      )
    )
    dispatch
  )
)
(define (push stack x)
  ((stack 'push) x)
)
(define (pop stack)
  (stack 'pop)
)
