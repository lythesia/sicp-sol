(define (mutex)
  (let (m (make-mutex))
    (define (me request)
      (cond
        ((eq? request 'acquire) (lock-mutex m)(display 'acquire-lock)(newline))
        ((eq? request 'release) (display 'release-lock)(newline)(unlock-mutex m))
        (else (error "Unknown operation -- MUTEX" request))
      )
    )
    me
  )
)

(define (make-serializer)
  (let ((m (mutex)))
    (lambda (f)
      (lambda (. args)
        (let ((x m 'acquire) (ret (apply f args)) (y m 'release)) ; exec left --> right
          ret
        )
      )
    )
  )
)

(define (parallel-execute . procs)
  (let ((threads (map call-with-new-thread procs)))
    (for-each join-thread threads)
  )
)
