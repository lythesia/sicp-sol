(load "util.scm") ; for make-table

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; generic 
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (square-g x) (apply-generic 'square-g x))
(define (sqrt-g x) (apply-generic 'sqrt-g x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (atangent y x) (apply-generic 'atangent y x))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))

; drop to proper type
(define (drop x)
  (let ((t (type-tag x)) (ct (contents x)))
    (if (and (eq? t 'scheme-number) (exact-integer? ct))
      x ; int no drop
      (let ((proj-x (project x)))
        (if (equ? (raise proj-x) x) (drop proj-x) x)
      )
    )
  )
)

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
  (let*
    ((type-tags (map type-tag args))
     (proc (get op type-tags))
    )
    (if proc
      ; drop result as proper as possible
      (let ((to-drop (or 
                       (eq? op 'add)
                       (eq? op 'sub)
                       (eq? op 'mul)
                       (eq? op 'div)
                       (eq? op 'square-g)
                       (eq? op 'sqrt-g)
                       (eq? op 'sine)
                       (eq? op 'cosine)
                       (eq? op 'atangent)
                     )))
        (if to-drop
          (drop (apply proc (map contents args)))
          (apply proc (map contents args))
        )
      )
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
(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)
  )
)
(define (type-tag o)
  (cond
    ((number? o) 'scheme-number)
    ((pair? o) (car o))
    (else (error "Bad tagged datnum -- TYPE-TAG" o))
  )
)
(define (contents o)
  (cond
    ((number? o) o)
    ((pair? o) (cdr o))
    (else (error "Bad tagged datnum -- CONTENTS" o))
  )
)

;; scheme-number
(define (install-scheme-number-package)
  ; <num>
  (define (tag x) (attach-tag 'scheme-number x))
  (let ((t 'scheme-number))
    (put 'add (list t t) (lambda (x y) (tag (+ x y))))
    (put 'sub (list t t) (lambda (x y) (tag (- x y))))
    (put 'mul (list t t) (lambda (x y) (tag (* x y))))
    (put 'div (list t t) (lambda (x y) (tag (/ x y))))
    (put 'sqrt (list t) (lambda (x) (tag (square x))))
    (put 'sqrt-g (list t) (lambda (x) (tag (sqrt x))))
    (put 'sine (list t) (lambda (x) (tag (sin x))))
    (put 'cosine (list t) (lambda (x) (tag (cos x))))
    (put 'atangent (list t t) (lambda (y x) (tag (atan y x))))
    (put 'equ? (list t t) =)
    (put '=zero? (list t) (lambda (x) (= x 0)))
    (put 'make t (lambda (x) (tag x)))
    ; int -> rat; float -> complex
    (put 'raise (list t) (lambda (x) (if (exact-integer? x) (make-rat x 1) (make-complex-from-real-imag x 0))))
    ; project real->rat(round numer/denom)
    (put 'project (list t)
      (lambda (x)
        (define eps 0.001)
        (define (try-rat nd)
          (let ((n (car nd)) (d (cadr nd)))
            (if (< (abs (/ (- n (round n)) n)) eps)
              (list (inexact->exact (round n)) d)
              (try-rat (list (* 10 n) (* 10 d)))
            )
          )
        )
        (cond
          ((exact-integer? x) x)
          ((= x 0) (make-rat 0 1))
          (else (let ((nd (try-rat (list x 1)))) (make-rat (car nd) (cadr nd))))
        )
      )
    )
  )
  'done
)

;; rational
(define (install-rational-package)
  ; (<tag> . (<numer> . <denom>))
  ; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))
    )
  )
  (define (add-rat x y)
    (make-rat
      (add (mul (numer x) (denom y)) (mul (numer y) (denom x)))
      (mul (denom x) (denom y))
    )
  )
  (define (sub-rat x y)
    (make-rat
      (sub (mul (numer x) (denom y)) (mul (numer y) (denom x)))
      (mul (denom x) (denom y))
    )
  )
  (define (mul-rat x y)
    (make-rat
      (mul (numer x) (numer y))
      (mul (denom x) (denom y))
    )
  )
  (define (div-rat x y)
    (make-rat
      (mul (numer x) (denom y))
      (mul (denom x) (numer y))
    )
  )
  (define (tag x) (attach-tag 'rational x))

  ; export
  (let ((t 'rational))
    (put 'add (list t t) (lambda (x y) (tag (add-rat x y))))
    (put 'sub (list t t) (lambda (x y) (tag (sub-rat x y))))
    (put 'mul (list t t) (lambda (x y) (tag (mul-rat x y))))
    (put 'div (list t t) (lambda (x y) (tag (div-rat x y))))
    ; added {{
    (put 'sqrt-g (list t) (lambda (x) (sqrt (/ (numer x) (denom x)))))
    (put 'square-g (list t) (lambda (x) (square-g (/ (numer x) (denom x)))))
    (put 'sine (list t) (lambda (x) (sine (/ (numer x) (denom x)))))
    (put 'cosine (list t) (lambda (x) (cosin (/ (numer x) (denom x)))))
    (put 'atangent (list t t) (lambda (y x) (atangent (/ (numer y) (denom y)) (/ (numer x) (denom x)))))
    ; }}
    (put 'equ? (list t t) (lambda (x y) (and (equ? (numer x) (numer y)) (equ? (denom x) (denom y)))))
    (put '=zero? (list t) (lambda (x) (=zero? (numer x))))
    (put 'make t (lambda (n d) (tag (make-rat n d))))
    ; rat -> real
    (put 'raise (list t) (lambda (x) (make-scheme-number (exact->inexact (/ (numer x) (denom x))))))
    ; rat -> int
    (put 'project (list t) (lambda (x) (make-scheme-number (round (/ (numer x) (denom x))))))
  )
  'done
)

; complex
; rectangular
(define (install-rectangualr-package)
  ; (<r> . <i>)
  ; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag r i) (cons r i))
  (define (make-from-mag-ang r a) (cons (mul r (cosine a)) (mul r (sine a))))
  (define (magnitude z)
    (sqrt-g (add (square-g (real-part z)) (square-g (imag-part z))))
  )
  (define (angle z) (atangent (imag-part z) (real-part z)))
  (define (tag x) (attach-tag 'rectangular x))

  ; export
  (let ((t 'rectangular))
    (put 'real-part (list t) real-part)
    (put 'imag-part (list t) imag-part)
    (put 'magnitude (list t) magnitude)
    (put 'angle (list t) angle)
    (put 'make-from-real-imag t (lambda (r i) (tag (make-from-real-imag r i))))
    (put 'make-from-mag-ang t (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'equ? (list t t) (lambda (x y) (and (equ? (real-part x) (real-part y)) (equ? (imag-part x) (imag-part y)))))
    (put '=zero? (list t) (lambda (x) (and (=zero? (real-part x)) (=zero? (imag-part x)))))
  )
  'done
)
; polar
(define (install-polar-package)
  ; (<r> . <a>)
  ; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z) (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z) (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag r i) 
    (cons (sqrt-g (add (square-g r) (square-g i))) (atangent i r))
  )
  (define (make-from-mag-ang r a) (cons r a))
  (define (tag x) (attach-tag 'polar x))

  ; export
  (let ((t 'polar))
    (put 'real-part (list t) real-part)
    (put 'imag-part (list t) imag-part)
    (put 'magnitude (list t) magnitude)
    (put 'angle (list t) angle)
    (put 'make-from-real-imag t (lambda (r i) (tag (make-from-real-imag r i))))
    (put 'make-from-mag-ang t (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'equ? (list t t) (lambda (x y) (and (equ? (magnitude x) (magnitude y)) (equ? (angle x) (angle y)))))
    (put '=zero? (list t) (lambda (x) (=zero? (magnitude x))))
  )
  'done
)

(define (install-complex-package)
  ; internal procedures
  (define (make-from-real-imag r i)
    ((get 'make-from-real-imag 'rectangular) r i)
  )
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a)
  )
  (define (add-complex x y)
    (make-from-real-imag
      (add (real-part x) (real-part y))
      (add (imag-part x) (imag-part y))
    )
  )
  (define (sub-complex x y)
    (make-from-real-imag
      (sub (real-part x) (real-part y))
      (sub (imag-part x) (imag-part y))
    )
  )
  (define (mul-complex x y)
    (make-from-mag-ang
      (mul (magnitude x) (magnitude y))
      (add (angle x) (angle y))
    )
  )
  (define (div-complex x y)
    (make-from-mag-ang
      (div (magnitude x) (magnitude y))
      (sub (angle x) (angle y))
    )
  )
  (define (tag x) (attach-tag 'complex x))
  (let ((t 'complex))
    (put 'add (list t t) (lambda (x y) (tag (add-complex x y))))
    (put 'sub (list t t) (lambda (x y) (tag (sub-complex x y))))
    (put 'mul (list t t) (lambda (x y) (tag (mul-complex x y))))
    (put 'div (list t t) (lambda (x y) (tag (div-complex x y))))
    (put 'make-from-real-imag t (lambda (r i) (tag (make-from-real-imag r i))))
    (put 'make-from-mag-ang t (lambda (r i) (tag (make-from-mag-ang r i))))
    (put 'real-part (list t) real-part)
    (put 'imag-part (list t) imag-part)
    (put 'magnitude (list t) magnitude)
    (put 'angle (list t) angle)
    (put 'equ? (list t t) equ?)
    (put '=zero? (list t) =zero?)
    ; complex -> real
    (put 'project (list t)
      (lambda (z) 
        (let* ((r (real-part z)) (up (- (type-level 1.0) (type-level r))))
          (make-scheme-number (raise-up up r))
        )
      )
    )
  )
  'done
)

; test
(install-scheme-number-package)
(install-rational-package)
(install-rectangualr-package)
(install-polar-package)
(install-complex-package)
; later export or will be #f
(define make-scheme-number (get 'make 'scheme-number))
(define (make-rat n d) ((get 'make 'rational) n d))
(define (make-complex-from-real-imag r i) ((get 'make-from-real-imag 'complex) r i))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))

; (display (raise (make-scheme-number 3)))(newline)
; (display (raise (make-scheme-number 3.14)))(newline)
; (display (raise (make-rat 3 4)))(newline)
; (display (raise (make-rat 6 3)))(newline)
; (define irat (make-rat 8 2))
; (display irat)(newline)
; (display (drop irat))(newline)
; (define icpx-a (make-complex-from-real-imag 2.5 0))
; (define icpx-b (make-complex-from-real-imag 3.5 0))
; (display (add icpx-a icpx-b))(newline)
; (define num-rat-to-complex (make-complex-from-real-imag 2 (make-rat 4 3)))
; (define num-num-to-complex (make-complex-from-mag-ang 3. 4))
; (display (add num-rat-to-complex num-num-to-complex))(newline)
; (display (add (make-rat 4 8) (make-complex-from-real-imag (make-rat 5 4) 10)))(newline) ; produce (rat 7 . 4) due to 1.75's drop
