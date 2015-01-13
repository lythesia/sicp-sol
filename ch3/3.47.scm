(load "3.04.02.mutex.scm")

; a)
(define (make-semaphore-mutex n)
  (let ((m (mutex)))
    (define (me request)
      (cond
        ((eq? request 'acquire)
         (m 'acquire)
         (cond
           ((= n 0) (m 'release) (me request))  ; release first and then try again
           (else (set! n (1- n) (m 'release)))  ; allow other threads
         )
        )
        ((eq? request 'release)
         (m 'acquire)
         (set! n (1+ n))
         (m 'release)
        )
        (else (error "Unknown operation -- SEMAPHORE" request))
      )
    )
    me
  )
)

; b)
(define (test-and-set! lock delta)
  (if (= (+ (car lock) delta) 0) #t
    (begin
      (set-car! lock (+ (car lock) delta))
      #f
    )
  )
)

(define (make-semaphore-mutex n)
  (let ((sm (list n)))
    (define (me request)
      (cond
        ((eq? request 'acquire)
         (if (test-and-set! sm -1) (me request))
        )
        ((eq? request 'release)
         (if (test-and-set! sm 1) (me request)) ; ?
        )
        (else "Unknown operation -- SEMAPHORE" request)
      )
    )
    me
  )
)
