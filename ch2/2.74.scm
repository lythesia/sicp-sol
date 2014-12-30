; a)
(define (ger-record employee generic-file)
  ((get 'get-record (division generic-file)) employee (original-file generic-file))
)
; tagged representation
(define (make-generic-file division file)
  (cons division file)
)
(define (division generic-file) (car generic-file))
(define (original-file generic-file) (cdr generic-file))

; b)
(define (get-salary generic-record)
  ((get 'get-salary (division generic-file)) (original-record generic-record))
)
(define (make-generic-record division record)
  (cons division record)
)
(define (division generic-record) (car generic-record))
(define (original-record generic-record) (cdr generic-record))

; c)
(define (find-employee-record employee files)
  (if (null? files)
    (error "unknown employee -- FIND-EMPLOYEE-RECORD" employee)
    (let ((f (car files)))
      (if (in-division? employee (division f))
        (get-record employee f)
        (find-employee-record employee (cdr files))
      )
    )
  )
)
(define (in-division? employee division)
  ((get 'in-division? division) employee)
)