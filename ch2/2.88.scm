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
(define (neg x) (apply-generic 'neg x))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))

; override
; temp no type promotioin and result type drop
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args)) (proc (get op type-tags)))
    (if proc
      (apply proc (map contents args))
      (error "No method for these types" (list op type-tags))
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
    (put 'neg (list t) (lambda (x) (tag (- x))))
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
    (put 'neg (list t) (lambda (x) (tag (make-rat (neg (numer x)) (denom x)))))
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

;; polynomial
(define (install-poly-package)
  ; <var . term>
  (define (make-poly var term-list)
    (cons var term-list)
  )
  (define (variable p) (car p))
  (define (term-list p) (cdr p)) ; as list
  (define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))
  (define (variable? x) (symbol? x))

  ; [<order,coe>]
  (define (make-term order coe) (list order coe))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))

  ; internal ops
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term)) term-list
      (cons term term-list) ; simply cons?
    )
  )
  (define (tag p) (attach-tag 'polynomial p))

  ; ops
  ; order h->l
  ; neg
  (define (neg-terms l)
    (if (empty-termlist? l) '()
      (let ((t (first-term l)))
        (cons
          (make-term (order t) (neg (coeff t)))
          (neg-terms (rest-terms l))
        )
      )
    )
  )
  (define (neg-poly p)
    (let ((v (variable p)) (l (term-list p)))
      (make-poly v (neg-terms l))
    )
  )
  ; add
  (define (add-terms l1 l2)
    (cond
      ((empty-termlist? l1) l2)
      ((empty-termlist? l2) l1)
      (else
        (let* ((t1 (first-term l1)) (t2 (first-term l2)) (o1 (order t1)) (o2 (order t2)))
          (cond
            ((> o1 o2) (adjoin-term t1 (add-terms (rest-terms l1) l2)))
            ((< o1 o2) (adjoin-term t2 (add-terms l1 (rest-terms l2))))
            (else
              (begin
                (adjoin-term
                  (make-term o1 (add (coeff t1) (coeff t2)))  ; term
                  (add-terms (rest-terms l1) (rest-terms l2)) ; rest-terms
                )
              )
            )
          )
        )
      )
    )
  )
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var -- ADD-POLY" (list p1 p2))
    )
  )
  ; mul
  (define (mul-terms l1 l2)
    (if (empty-termlist? l1) (empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term l1) l2) (mul-terms (rest-terms l1) l2))
    )
  )
  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l) (empty-termlist)
      (let ((t2 (first-term l)))
        (adjoin-term
          (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms l))
        )
      )
    )
  )
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var -- MUL-POLY" (list p1 p2))
    )
  )

  ; export
  (let ((t 'polynomial))
    (put 'add (list t t) (lambda (x y) (tag (add-poly x y))))
    (put 'sub (list t t) (lambda (x y) (tag (add-poly x (neg-poly y)))))
    (put 'mul (list t t) (lambda (x y) (tag (mul-poly x y))))
    (put '=zero? (list t)
      (lambda (p) (empty-termlist? (term-list p)))
    )
    (put 'neg (list t) (lambda (p) (tag (neg-poly p))))
    (put 'make t (lambda (var term-list) (tag (make-poly var term-list))))
  )
  'done
)

; test
(install-scheme-number-package)
(install-rational-package)
(install-rectangualr-package) ; no neg
(install-polar-package)       ; no neg
(install-complex-package)     ; no neg
(install-poly-package)
; later export or will be #f
(define make-scheme-number (get 'make 'scheme-number))
(define make-rat (get 'make 'rational))
(define make-complex-from-real-imag (get 'make-from-real-imag 'complex))
(define make-complex-from-mag-ang (get 'make-from-mag-ang 'complex))
(define make-polynomial (get 'make 'polynomial))

(define ca (make-polynomial 'x '((2 2) (1 3) (0 1))))
(define cb (make-polynomial 'x '((3 4) (1 1))))
(define cc (make-polynomial 'x '((1 1) (0 10))))
(define cd (make-polynomial 'x '((3 -4) (1 -1))))
(define p1 (make-polynomial 'y (list (list 2 ca) 
                                     (list 1 cb) 
                                     (list 0 cc))))
(define p2 (make-polynomial 'y (list (list 1 ca) 
                                     (list 0 cb)))) 
(define p3 (make-polynomial 'y (list (list 2 ca) 
                                     (list 1 cd) 
                                     (list 0 cc))))
; (display ca)(newline)
; (display (neg cb))(newline)
; (display (sub ca cb))(newline)
