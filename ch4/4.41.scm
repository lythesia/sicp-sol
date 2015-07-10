(include "4.03.02.common.scm")

(define (next-permutation lst)
  (let ((n (length lst)))
    ;; find turning-point i, where i < i + 1 > i + 2
    (define (index-turning-point)
      (define (iter try-turning-point)
        (cond
          ((< try-turning-point 0)
           '() ; nil
          )
          ((< (list-ref lst try-turning-point) (list-ref lst (+ try-turning-point 1)))
           try-turning-point
          )
          (else
            (iter (- try-turning-point 1))
          )
        )
      )
      (iter (- n 2))  ; (n-2, n-1) ... (0, 1)
    )
    (let ((turning-point (index-turning-point)))
      ;; find swap point j, where j is just > i(turning-point)
      (define (index-swap-point)
        (define (iter try-swap-point)
          (if (< (list-ref lst turning-point) (list-ref lst try-swap-point))
            try-swap-point
            (iter (- try-swap-point 1))
          )
        )
        (iter (- n 1))
      )
      ;; main proc here
      (if (null? turning-point)
        '()
        (let ((swap-point (index-swap-point)))
          (let ((swapped-list (list-swap lst turning-point swap-point))
                (new-asc-len (+ turning-point 1)))
            (append
              (list-head swapped-list new-asc-len) ; 0..i; (list-head lst k) returns first k elements
              (reverse (list-tail swapped-list new-asc-len)); i+1..end; (list-tail lst k) skips first k elements
            )
          )
        )
      )
    )
  )
)

; test next-permutation
; (display (next-permutation (list 1 2 4 3)))(newline) ; (1 3 2 4)

(define (multiple-dwelling)
  (define (valid? permutation)
    (let
      ((baker     (first permutation))
       (cooper    (second permutation))
       (fletcher  (third permutation))
       (miller    (fourth permutation))
       (smith     (fifth permutation))
      )
      (and
        (!= baker 5)
        (!= cooper 1)
        (!= fletcher 5)
        (!= fletcher 1)
        (> miller cooper)
        (!= (abs (- smith fletcher)) 1)
        (!= (abs (- cooper fletcher)) 1)
      )
    )
  )
  (define (try permutation)
    (if (pair? permutation)
      (if (valid? permutation)
        (map (lambda (name level) (list name level)) '(baker cooper fletcher miller smith) permutation)
        (try (next-permutation permutation))
      )
      '() ; no solution
    )
  )
  (try (list 1 2 3 4 5))
)

; test
; (display (multiple-dwelling))(newline) ; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
