(load "3.03.02.scm") ; for queue (non-dispatch)

; <time-interval . action-queue> action should be performed during time-interval
(define (make-time-segment time q)
  (cons time q)
)
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

; agenda
; [curren-time, <seg> .. ]
(define (make-agenda) (list 0))
(define (current-time a) (car a))
(define (set-current-time! a t) (set-car! a t))
(define (segments a) (cdr a))
(define (set-segments! a segs) (set-cdr! a segs))
(define (first-segment a) (car (segments a)))
(define (rest-segments a) (cdr (segments a)))
(define (empty-agenda? a) (null? (segments a)))

; time asc order
; curren-time
;   |
;   v
; time1 q:[act1, act2, ..]
;   |
;   v
; time2 q:[act1', act2', ..]
;   |
;   v
;  ... 
(define (add-to-agenda! time action agenda)
  ; should be placed at beginning
  (define (belongs-before? segs)
    (or (null? segs) (< time (segment-time (car segs))))
  )
  ; make a new segment
  (define (make-new-time-seg time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)
    )
  )
  ; segs assert not null
  (define (add-to-segments! segs)
    (if (= (segment-time (car segs)))
      (insert-queue! (segment-queue (car segs)) action)
      (let ((rest (cdr segs)))
        (if (belongs-before? rest)
          (set-cdr! segs (cons (make-new-time-seg time action) (cdr segs)))
          (add-to-segments! rest)
        )
      )
    )
  )
  (let ((segs (segments agenda)))
    (if (belongs-before? segs)
      (set-segments! agenda (cons (make-new-time-seg time action) segs))
      (add-to-segments! segs)
    )
  )
)

(define (remove-first-agenda-item! agenda)
  (if (empty-agenda? agenda) agenda
    (let ((q (segment-queue (first-segment agenda))))
      (delete-queue! q)
      (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda))
      )
    )
  )
)

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIRST-AGENDA-ITEM" agenda)
    (let ((first-seg (car (segments agenda))))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg))
    )
  )
)

(define (print-agenda agenda)
  (define (print-segments segs)
    (define (print-queue q)
      (if (null? q) (newline)
        (begin
          (display "  ")
          (display (car q))(newline)
          (print-queue (cdr q))
        )
      )
    )

    (if (null? segs) (display "over.\n")
      (begin
        (display "time: ")
        (display (segment-time (car segs)))
        (newline)
        (print-queue (segment-queue (car segs)))
        (newline)
        (print-segments (cdr segments))
      )
    )
  )
  (begin
    (display "current-time: ")
    (display (current-time agenda))
    (newline)
    (print-segments (segments agenda))
  )
)
