(define memorize
  (lambda (f)
    ((lambda (table)
      (lambda (x) ; single x for f
        ((lambda (prev-computed-result)
           (or prev-computed-result
               ((lambda (result)
                  (insert! x result table)
                  result
                )
                (f x)
               )
           )
         )
         (lookup x table)
        )
      )
     )
     (make-table)
    )
  )
)

(define memo-fib
  (memorize
    (lambda (n)
      (cond
        ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (memo-fib (- n 1)) (memo-fib (- n 2))))
      )
    )
  )
)

; now expand
; 1. memo-fib
((lambda (f)
  ((lambda (table)
    (lambda (x) ; single x for f
      ((lambda (prev-computed-result)
         (or prev-computed-result
             ((lambda (result)
                (insert! x result table)
                result
              )
              (f x)
             )
         )
       )
       (lookup x table)
      )
    )
   )
   (make-table)
  )
 ) ; body
 (lambda (n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (memo-fib (- n 1)) (memo-fib (- n 2))))
  )
 ) ; arg
)

; 2. expand arg to body
((lambda (table)
  (lambda (x) ; single x for f
    ((lambda (prev-computed-result)
       (or prev-computed-result
           ((lambda (result)
              (insert! x result table)
              result
            ) 
            ((lambda (n)
               (cond
                 ((= n 0) 0)
                 ((= n 1) 1)
                 (else (+ (memo-fib (- n 1)) (memo-fib (- n 2 ))))
               )
             )
             x
            )
           )
       )
     )
     (lookup x table)
    )
  )
 )
 (make-table)
)

; 3. expand table
(lambda (x) ; single x for f
  ((lambda (prev-computed-result)
     (or prev-computed-result
         ((lambda (result)
            (insert! x result table)
            result
          ) 
          ((lambda (n)
             (cond
               ((= n 0) 0)
               ((= n 1) 1)
               (else (+ (memo-fib (- n 1)) (memo-fib (- n 2 ))))
             )
           )
           x
          )
         )
     )
   )
   (lookup x table)
  )
)
; this is memo-fib


;           +-----------------------------------------------------------------------------------------------------+
;           |                                                                                                     |
; global -> | memo-fib                                                                                            |
; env       |  |                                                                                                  |
;           +--|--------------------------------------------------------------------------------------------------+
;              |                    ^
;              |                    |
;              |  (lambda (f) ...)  |
;              |                    |
;              |                 +-----+
;              |                 |     |
;              |                 |     |<---------+
;              |                 |     |          |
;              |                 |  f ------->[*][*]
;              |                 |     |    parameters: n
;              |                 +-----+    body: (cond ((= n 0)
;              |                   ^                     0)
;              |                   |                   ((= n 1)
;              |                   |                     1)
;              |  (lambda (table)  |                   (else
;              |      ...)         |                     (+ (memo-fib (- n 1))
;              |                   |                        (memo-fib (- n 2)))))
;              |                   |
;              |               +-------+
;              |               |       |
;              |               |       |<------------------------------------------+
;              |               |       |                                           |
;              |               | table -----------------------------------+    +------------------------------+
;              |               |       |                                  |    |                              |
;              |               +-------+                                  |    | local-table: (list '*table*) |
;              |                 |  ^                                     |    |                              |
;              |                 |  |                                     |    | assoc                        |
;              +--------------->[*][*]                                    |    | lookup                       |
;                                |                                        |    | insert!                      |
;                                |                                        +----->dispatch                     |
;                                |                                             |                              |
;                                |                                             +------------------------------+
;                                v
;                     parameters: x
;                     body: ((lambda (previously-computed-result)
;                               (or previously-computed-result
;                                   ((lambda (result)
;                                      (insert! x result table)
;                                      result)
;                                    (f x))))
;                            (lookup x table))


;; when apply 3
;           +-----------------------------------------------------------------------------------------------------+
;           |                                                                                                     |
; global -> | memo-fib                                                                                            |
; env       |  |                                                                                                  |
;           +--|--------------------------------------------------------------------------------------------------+
;              |                    ^
;              |                    |
;              |  (lambda (f) ...)  |
;              |                    |
;              |                 +-----+
;              |                 |     |
;              |                 |     |<---------+
;              |                 |     |          |
;              |                 |  f ------->[*][*]
;              |                 |     |    parameters: n
;              |                 +-----+    body: (cond ((= n 0)
;              |                   ^                     0)
;              |                   |                   ((= n 1)
;              |                   |                     1)
;              |  (lambda (table)  |                   (else
;              |      ...)         |                     (+ (memo-fib (- n 1))
;              |                   |                        (memo-fib (- n 2)))))
;              |                   |
;              |               +-------+
;              |               |       |
;              |               |       |<------------------------------------------+
;              |+--------------|       |                                           |
;              vv              | table -----------------------------------+    +------------------------------+
;              [*][*]--------->|       |                                  |    |                              |
;               |              +-------+                                  |    | local-table: (list '*table*) |
;               v                ^                                        |    |                              |
;       parameters: x            |                                        |    | assoc                        |
;       body: ...                |                                        |    | lookup                       |
;                                |                                        |    | insert!                      |
;                                |                                        +----->dispatch                     |
;            (lambda (x)         |                                             |                              |
;                ...)            |                                             +------------------------------+
;                                |
;                            +------+
;                            |      |
;                            | x: 3 |
;                            |      |
;                            +------+
;                                ^
;                                |
; (lambda                        |
;   (previously-computed-result) |
;   ...)                         |
;                                |
;                                |
;                            +----------------------------------------------+
;                            |                                              |
;                            | previously-computed-result: (lookup x table) |
;                            |                                              |
;                            +----------------------------------------------+
;                             (or previously-computed-result
;                                 ((lambda (result)
;                                     (insert! x result table)
;                                     result)
;                                   (f x)))


;; after apply 3
;           +--------------------+
;           |                    |
; global -> |   memo-fib         |
; env       |                    |
;           +--------------------+
;                     ^
;                     |
;   (lambda (f) ...)  |
;                     |
;                 +-----+
;                 |     |
;                 |     |<---------+
;                 |     |          |
;                 |  f ------->[*][*]
;                 |     |    parameters: n
;                 +-----+    body: (cond ((= n 0)
;                    ^                     0)
;                    |                   ((= n 1)
;                    |                     1)
;   (lambda (table)  |                   (else
;       ...)         |                     (+ (memo-fib (- n 1))
;                    |                        (memo-fib (- n 2)))))
;                    |
;                 +-------+
;                 |       |
;                 |       |<------------------------------------------+
;                 |       |                                           |
;                 | table -----------------------------------+    +---------------------------------------------------------------+
;                 |       |                                  |    |                                                               |
;                 +-------+                                  |    | local-table: (list '*table* (cons 3 2) (cons 2 1) (cons 1 1)) |
;                   |  ^                                     |    |                                                               |
;                   |  |                                     |    | assoc                                                         |
;                  [*][*]                                    |    | lookup                                                        |
;                   |                                        |    | insert!                                                       |
;                   |                                        +----->dispatch                                                      |
;                   |                                             |                                                               |
;                   |                                             +---------------------------------------------------------------+
;                   v
;            parameters: x
;            body: ((lambda (previously-computed-result)
;                      (or previously-computed-result
;                          ((lambda (result)
;                              (insert! x result table)
;                              result)
;                           (f x))))
;                   (lookup x table))


;; if (define memo-fib (memorize fib)), then in let ((result (f x))) where f is fib, fib calls itself NOT the memo-fib, so the result is not cached!
;; but the current result will be stored
;; take memo-fib 3, after computing, fib(3) is stored
;; then if use x > 3, take 4, = memo-fib(3) + memo-fib(2), where memo-fib(3) is lookup from table, while memo-fib(2) need re-compute
;;
;; key is that the arg to memorize must call the memorized version function
;; (define want-to-memorized <--+
;;  (memorize                   |
;;    (lambda (..)              | call 
;;      .. ---------------------+
;;    )
;;  )
;; )

;; refer v[0-3] for compare
