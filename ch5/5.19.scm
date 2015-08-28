(use-modules (ice-9 format))
(include "util.scm")

;; register
(define (make-register name)
  (let ((contents '*unassigned*) (watch? #f))
    (define (dispatch message)
      (cond
        ((eq? message 'trace-on) (begin (set! watch? #t) (format #t "watch set: ~a" name)))
        ((eq? message 'trace-off) (begin (set! watch? #f) (format #t "watch unset: ~a" name)))
        ((eq? message 'get) contents)
        ((eq? message 'set)
         (lambda (value)
           (if watch? (format #t "~a: ~a -> ~a\n" name contents value))
           (set! contents value)
         )
        )
        (else (error "Unknown request -- REGISTER" message))
      )
    )
    dispatch
  )
)
(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))
(define (watch register) (register 'trace-on))
(define (rwatch register) (register 'trace-off))

;; stack
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    ; push
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ number-pushes 1))
      (set! current-depth (+ current-depth 1))
      (set! max-depth (max max-depth current-depth))
    )
    ; pop
    (define (pop)
      (if (null? s)
        (error "Empty stack -- POP")
        (let ((top (car s)))
          (set! s (cdr s))
          (set! current-depth (- current-depth 1))
          top
        )
      )
    )
    ; init
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done
    )
    ; profile
    (define (profile)
      (format #t "\ntotal-pushes: ~a, maximum-depth: ~a\n" number-pushes max-depth)
    )
    (define (dispatch message)
      (cond
        ((eq? message 'push) push)
        ((eq? message 'pop) (pop))
        ((eq? message 'initialize) (initialize))
        ((eq? message 'print-statistics) (profile))
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

;; breakpoint
;; (label-exist-in-code relative-offset absolute-offset)
(define (make-breakpoint label offset labels)
  (let ((val (assoc label labels)))
    (if val (list label offset (+ offset (label-entry-count val)))
      (error "Unknown label -- MAKE-BREAKPOINT" label))
  )
)
(define breakpoint-label car)
(define breakpoint-offset cadr)
(define breakpoint-absolute caddr)

;; machine
(define (make-machine ops controller-text) ; text = code
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence) (assemble controller-text machine))
    machine
  )
)

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ;; support breakpoint
        (labels '())
        (breakpoints '()))
    (let ((the-ops
            (list (list 'initialize-stack (lambda () (stack 'initialize)))
                  (list 'print-stack-statistics (lambda () (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      ; allocate
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "register redefined:" name)
          (let ((new-register (make-register name)))
            (set! register-table (cons (list name new-register) register-table))
            new-register
          )
        )
      )
      ; lookup-register
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val (cadr val) (allocate-register name))
        )
      )
      ; execute
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts) 'done
            (begin
              (let
                ; slow since checks all breakpoints each execute step
                ((anybreaks (filter
                      (lambda (break) (= (breakpoint-absolute break) (instruction-line-count (car insts))))
                      breakpoints)))
                (if (null? anybreaks)
                  ; execute rests
                  (execute-continue)
                  ; display breakpoints and exit `execute`
                  (begin
                    (display "breakpoint hit: ")
                    (for-each (lambda (break) (format #t "~a + ~aLN  " (breakpoint-label break) (breakpoint-offset break))) anybreaks)
                    (newline)
                  )
                )
              )
            )
          )
        )
      )
      ; execute aux
      (define (execute-continue)
        (let ((insts (get-contents pc)))
          ((instruction-execution-proc (car insts)))
          (execute)
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
          ;; support breakpoint
          ((eq? message 'set-breakpoint!)
           (lambda (label offset) (set! breakpoints (cons (make-breakpoint label offset labels) breakpoints))))
          ((eq? message 'cancel-breakpoint!)
           (lambda (label offset) (set! breakpoints (filter
                                                      (lambda (break) (not (and (eq? (breakpoint-label break) label))
                                                                           (and (eq? (breakpoint-offset break) offset))))
                                                      breakpoints))))
          ((eq? message 'cancel-all-breakpoints!) (lambda () (set! breakpoints '())))
          ((eq? message 'proceed) (lambda () (execute-continue)))
          ((eq? message 'set-labels!) (lambda (ls) (set! labels ls)))
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
(define (set-breakpoint! machine label offset) ((machine 'set-breakpoint!) label offset))
(define (cancel-breakpoint! machine label offset) ((machine 'cancel-breakpoint!) label offset))
(define (cancel-all-breakpoints! machine) ((machine 'cancel-all-breakpoints!)))
(define (proceed-machine machine) ((machine 'proceed)))

;; assemble
(define (assemble controller-text machine)
  (extract-labels controller-text (lambda (insts labels) (update-insts! insts labels machine) insts))
)

(define (extract-labels text receive)
  (define (extract-labels-with-count count text receive)
    (if (null? text)
      (receive '() '())
      (extract-labels-with-count
        (if (symbol? (car text)) count (+ count 1)) ;; not count labels
        (cdr text)
        (lambda (insts labels)
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
              (receive insts (cons (make-label-entry next-inst insts count) labels))
              (receive (cons (make-instruction next-inst count) insts) labels)
            )
          )
        )
      )
    )
  )
  (extract-labels-with-count 0 text receive)
)
; inst: (code-text exec-proc line-count)
(define (make-instruction text count) (list text '() count))
(define instruction-text car)
(define instruction-execution-proc cadr)
(define instruction-line-count caddr)
(define (set-instruction-execution-proc! inst proc) (set-car! (cdr inst) proc))
; label: (label-text  all-code-text-after line-count)
(define (make-label-entry label-name insts count) (list label-name insts count))
(define label-entry-name car)
(define label-entry-insts cadr)
(define label-entry-count caddr)
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val (label-entry-insts val) (error "Undefined label -- ASSEMBLE" label-name))
  )
)

(define (update-insts! insts labels machine)
  (let
    ((pc (get-register machine 'pc))
     (flag (get-register machine 'flag))
     (stack (machine 'stack))
     (ops (machine 'operations)))
    ((machine 'set-labels!) labels)
    (for-each
      (lambda (inst)
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst)
            labels machine pc flag stack ops))
      )
      insts
    )
  )
)

;; make-exec
(include "5.02.03.inst-proc.scm")
