; (a)
(define (make-mobile l r) (list l r))

(define (left-branch t) (car t))
(define (right-branch t) (cadr t))

(define (make-branch len struct) (list len struct))

(define (branch-len b) (car b))
(define (branch-struct b) (cadr b))

; (b)
(define (branch-weight br)
  (if (hang-struct? br) (total-weight (branch-struct br)) ; since branch always weights on right
    (branch-struct br)
  )
)

(define (hang-struct? br)
  (pair? (branch-struct br))
)
  
(define (total-weight b)
  (+ (branch-weight (left-branch b)) (branch-weight (right-branch b)))
)

; test
(define m1 (make-mobile (make-branch 10 20) (make-branch 10 25)))
(define m1plus (make-mobile (make-branch 10 m1) (make-branch 10 20)))
; (display (total-weight m1plus))(newline)

; (c)
(define (branch-torque br) (* (branch-len br) (branch-weight br)))

(define (branch-balance? br)
  (if (hang-struct? br)
    (mobile-balance? (branch-struct br)) ; same reason
    #t
  )
)

(define (mobile-balance? b)
  (let ((left (left-branch b)) (right (right-branch b)))
    (and
      (branch-balance? left)
      (branch-balance? right)
      (= (branch-torque left) (branch-torque right))
    )
  )
)

; test
; (display (balance? m1plus))(newline)

; (d)
; (define (make-mobile l r) (cons l r))
; (define (right-branch b) (cdr b))

; (define (make-branch len struct) (cons len struct))
; (define (branch-struct br) (cdr br))
