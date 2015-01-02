(define (make-deque) (cons '() '()))
(define (front-ptr dq) (car dq))
(define (rear-ptr dq) (cdr dq))

(define (empty-deque? dq) (null? (front-ptr dq)))
(define (front-deque dq)
  (if (empty-deque? dq) (error "empty deque -- FRONT-DEQUE" dq) (car (front-ptr dq)))
)
(define (rear-deque dq)
  (if (empty-deque? dq) (error "empty deque -- REAR-DEQUE" dq) (car (rear-ptr dq)))
)

(define (set-front-ptr! dq i) (set-car! dq i))
(define (set-rear-ptr! dq i) (set-cdr! dq i))
(define (make-node i prev next) (cons i (cons prev next)))
(define (from-null-aux! dq i)
  (let ((initial (make-node i '() '())))
    (set-front-ptr! dq initial)
    (set-rear-ptr! dq initial)
  )
)
(define (front-insert-deque! dq i)
  (cond
    ((empty-deque? dq) (from-null-aux! dq i) dq)
    (else
      (let* ((fp (front-ptr dq)) (new (make-node i '() fp)))
        (set-car! (cdr fp) new) ; new <- front(prev)
        (set-front-ptr! dq new) ; front as new
        dq
      )
    )
  )
)
(define (rear-insert-deque! dq i)
  (cond
    ((empty-deque? dq) (from-null-aux! dq i) dp)
    (else
      (let* ((rp (rear-ptr dq)) (new (make-node i rp '())))
        (set-cdr! (cdr rp) new) ; rear(next) -> new
        (set-rear-ptr! dq new)  ; rear as new
        dq
      )
    )
  )
)
(define (front-delete-deque! dq)
  (cond
    ((empty-deque? dq) (error "empty deque -- FRONT-DELETE-DEQUE!"))
    ((eq? (front-ptr dq) (rear-ptr dq)) (set-front-ptr! dq '()) (set-rear-ptr! dq '()) dq)
    (else
      (set-front-ptr! dq (cddr (front-ptr dq)))
      (set-car! (cdr (front-ptr dq)) '())
      dq
    )
  )
)
(define (rear-delete-deque! dq)
  (cond
    ((empty-deque? dq) (error "empty deque -- REAR-DELETE-DEQUE!"))
    ((eq? (front-ptr dq) (rear-ptr dq)) (set-front-ptr! dq '()) (set-rear-ptr! dq '()) dq)
    (else
      (set-rear-ptr! dq (cadr (rear-ptr dq)))
      (set-cdr! (cdr (rear-ptr dq)) '())
      dq
    )
  )
)

(define (print-deque dq)
  (define (make-printable-list q)
    (if (null? q) '()
      (cons (car q) (make-printable-list (cddr q)))
    )
  )
  (newline)
  (display (make-printable-list (front-ptr dq)))
)

; test
; (define q1 (make-deque))
; (front-insert-deque! q1 'a)
; (print-deque q1)
; ; (a)
; (front-insert-deque! q1 'b)
; (print-deque q1)
; ; (b a)
; (rear-insert-deque! q1 'x)
; (print-deque q1)
; ; (b a x)
; (rear-insert-deque! q1 'y)
; (print-deque q1)
; ; (b a x y)
; (rear-delete-deque! q1)
; (print-deque q1)
; ; (b a x)
; (front-delete-deque! q1)
; (print-deque q1)
; ; (a x)
; (front-delete-deque! q1)
; (print-deque q1)
; ; (x)
; (front-delete-deque! q1)
; (print-deque q1)
; (newline)
; ; ()
; (display (empty-deque? q1))(newline)
; ;Value: #t
