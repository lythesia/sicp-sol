(load "util.scm") ; for make-table

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; generic 
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (equ x y) (apply-generic 'equ x y))
(define (zero? x) (apply-generic 'zero? x))
(define (raise x) (apply-generic 'raise x))

(define (apply-generic op . args)
  (let*
    ((type-tags (map type-tag args))
     (proc (get op type-tags))
    )
    (if proc
      (apply proc (map contents args))
      (error "No method for these types -- APPLY-GENERIC" (list op type-tags))
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
    (put 'equ (list t t) =)
    (put 'zero? (list t) (lambda (x) (= x 0)))
    (put 'make t (lambda (x) (tag x)))
    ; int -> rat; float -> complex
    (put 'raise (list t) (lambda (x) (if (exact-integer? x) (make-rat x 1) (make-complex-from-real-imag x 0))))
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
      (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
      (* (denom x) (denom y))
    )
  )
  (define (sub-rat x y)
    (make-rat
      (- (* (numer x) (denom y)) (* (numer y) (denom x)))
      (* (denom x) (denom y))
    )
  )
  (define (mul-rat x y)
    (make-rat
      (* (numer x) (numer y))
      (* (denom x) (denom y))
    )
  )
  (define (div-rat x y)
    (make-rat
      (* (numer x) (denom y))
      (* (denom x) (numer y))
    )
  )
  (define (tag x) (attach-tag 'rational x))

  ; export
  (let ((t 'rational))
    (put 'add (list t t) (lambda (x y) (tag (add-rat x y))))
    (put 'sub (list t t) (lambda (x y) (tag (sub-rat x y))))
    (put 'mul (list t t) (lambda (x y) (tag (mul-rat x y))))
    (put 'div (list t t) (lambda (x y) (tag (div-rat x y))))
    (put 'equ (list t t) (lambda (x y) (and (equ (numer x) (numer y)) (equ (denom x) (denom y)))))
    (put 'zero? (list t) (lambda (x) (zero? (numer x))))
    (put 'make t (lambda (n d) (tag (make-rat n d))))
    ; rat -> real
    (put 'raise (list t) (lambda (x) (make-scheme-number (exact->inexact (/ (numer x) (denom x))))))
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
  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z))))
  )
  (define (angle z) (atan (imag-part z) (real-part z)))
  (define (tag x) (attach-tag 'rectangular x))

  ; export
  (let ((t 'rectangular))
    (put 'real-part (list t) real-part)
    (put 'imag-part (list t) imag-part)
    (put 'magnitude (list t) magnitude)
    (put 'angle (list t) angle)
    (put 'make-from-real-imag t (lambda (r i) (tag (make-from-real-imag r i))))
    (put 'make-from-mag-ang t (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'equ (list t t) (lambda (x y) (and (equ (real-part x) (real-part y)) (equ (imag-part x) (imag-part y)))))
    (put 'zero? (list t) (lambda (x) (and (zero? (real-part x)) (zero? (imag-part x)))))
  )
  'done
)
; polar
(define (install-polar-package)
  ; (<r> . <a>)
  ; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag r i) 
    (cons (sqrt (+ (square r) (square i))) (atan i r))
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
    (put 'equ (list t t) (lambda (x y) (and (equ (magnitude x) (magnitude y)) (equ (angle x) (angle y)))))
    (put 'zero? (list t) (lambda (x) (zero? (magnitude x))))
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
      (+ (real-part x) (real-part y))
      (+ (imag-part x) (imag-part y))
    )
  )
  (define (sub-complex x y)
    (make-from-real-imag
      (- (real-part x) (real-part y))
      (- (imag-part x) (imag-part y))
    )
  )
  (define (mul-complex x y)
    (make-from-mag-ang
      (* (magnitude x) (magnitude y))
      (+ (angle x) (angle y))
    )
  )
  (define (div-complex x y)
    (make-from-mag-ang
      (/ (magnitude x) (magnitude y))
      (- (angle x) (angle y))
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
    (put 'equ (list t t) equ)
    (put 'zero? (list t) zero?)
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
