;; in make-new-machine
(define (lookup-register name)
  (let ((val (assoc name register-table))) ; entry
    (if val
      (cadr val) ; register object
      (begin
        (allocate-register name)
        (lookup-register name) ; or (car register-table) since it's head placed
      )
      ;; or we can make (allocate-register name) return the made register
    )
  )
)
