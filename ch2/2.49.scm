(load "2.46.scm")
(load "2.47.scm")
(load "2.48.scm")

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame)) (scale-vect (ycor-vect v) (edge2-frame frame)))
    )
  )
)

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line ((frame-coord-map frame) (start-segment segment)) ((frame-coord-map frame) (end-segment segment)))
      )
      segment-list
    )
  )
)

; border
(define top-left (make-vect 0.0 1.0))
(define top-right (make-vect 1.0 1.0))
(define bottom-left (make-vect 0.0 0.0))
(define bottom-right (make-vect 1.0 0.0))
(define top (make-segment top-left top-right))
(define left (make-segment top-left bottom-left))
(define right (make-segment top-right bottom-right))
(define bottom (make-segment bottom-left bottom-right))
(define border-painter (segments->painter (list top bottom left right)))

; cross
(define left-top-to-right-bottom (make-segment top-left bottom-right))
(define right-top-to-left-bottom (make-segment top-right bottom-left))
(define cross-painter (segments->painter (list left-top-to-right-bottom right-top-to-left-bottom)))

; diamond
(define top-mid-point (make-vect 0.5 1.0))
(define bottom-mid-point (make-vect 0.5 0.0))
(define left-mid-point (make-vect 0.0 0.5))
(define right-mid-point (make-vect 1.0 0.5))
(define top-to-left (make-segment top-mid-point
(define top-to-right (make-segment top-mid-point right-mid-point))
(define bottom-to-left (make-segment bottom-mid-point left-mid-point))
(define bottom-to-right (make-segment bottom-mid-point right-mid-point))
(define diamond-painter (segment->painter (list top-to-left top-to-right bottom-to-left bottom-to-right)))
