;; recall 3.77
; (define (integral delayed-integrand initial-value dt)
;   (cons-stream
;     initial-value
;     (let ((integrand (force delayed-integrand)))    <- since delayed-integrand from this point before may be 
;       (if (empty-stream? integrand)                    un-constructed, so we should delay it first, when we
;         the-empty-stream                               need it, we force it
;         (integral
;           (delay (stream-cdr integrand))            <- as we said before, we should delay it first
;           (+ (* (stream-car integrand) dt) initial-value)
;           dt
;         )
;       )
;     )
;   )
; )

; (define (solve f y0 dt)
;   (define y (integral (delay dy) y0 dt))    <- note this dy is not defined yet at this point, we delay it
;   (define dy (stream-map f y))              <- ok, we define dy here, which depends on f, wtf? f?
;   y                                            yes, f has a valid first item, that is y0!
; )                                              so, y 和 dy 是相互依赖, 交替构造的.
;

;; lazy ver
(define (integral integrand initial-value dt)
  (define int
    (cons initial-value
          (add-lists (scale-list integrand dt)))
  )
  int
)
(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map f y))
  y
)
;; but with lazier lazy list, the list(stream) derived from `cons` `add/scale-lists` `map` as born lazy, so we
;; need not to delay it by hand, also they are evaluated when needed, we need not to force it neither
