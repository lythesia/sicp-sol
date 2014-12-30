(load "2.83.scm")

; 0: scheme-number int
; 1: rational
; 2: scheme-number real
; 3: complex
(define (type-level x)
  (let ((type (type-tag x)))
    (cond
      ((eq? type 'rational) 1)
      ((eq? type 'complex) 3)
      (else
        (let ((val (contents x)))
          (if (exact-integer? val) 0 2)
        )
      )
    )
  )
)

; raise lv types
(define (raise-up lv x)
  (if (= lv 0) x
    (raise-up (1- lv) (raise x))
  )
)

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args)) (proc (get op type-tags)))
    (if proc
      ; directly
      (apply proc (map contents args))
      ; type promotion
      (if (= (length args) 2)
        (let* ((a1 (car args)) (a2 (cadr args)) (l1 (type-level a1)) (l2 (type-level a2)))
          (cond
            ((< l1 l2) (apply-generic op (raise-up (- l2 l1) a1) a2))
            (else (apply-generic op a1 (raise-up (- l1 l2) a2)))
          )
        )
        (error "# arg more than 2 -- APPLY-GENERIC" (list op type-tags))
      )
    )
  )
)

; (display (sub (make-scheme-number 10) (make-rat 3 4)))(newline)
; (display (mul (make-rat 4 3) (make-complex-from-mag-ang 3.0 0.55)))(newline)
