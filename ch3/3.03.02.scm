; queue struct
(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q i) (set-car! q i))
(define (set-rear-ptr! q i) (set-cdr! q i))
(define (make-queue) (cons '() '()))

; queue util
; select
(define (empty-queue? q) (null? (front-ptr q)))
(define (front-queue q) (if (empty-queue? q) (error "FRONT empty queue" queue) (car (front-ptr q))))
; mutable
(define (insert-queue! q i)
  (let ((new (cons i '())))
    (cond
      ((empty-queue? q) (set-front-ptr! q new) (set-rear-ptr! q new) q)
      (else (set-cdr! (rear-ptr q) new) (set-rear-ptr! q new) q)
    )
  )
)
(define (delete-queue! q)
  (cond
    ((empty-queue? q) (error "DELETE empty queue" q))
    (else (set-front-ptr! q (cdr (front-ptr q))) q)
  )
)
