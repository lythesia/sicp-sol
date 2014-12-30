(load "2.86.scm")

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
  ; add
  (define (add-terms l1 l2)
    (cond
      ((empty-termlist? l1) l2)
      ((empty-termlist? l2) l1)
      (else
        (let* ((t1 (first-term l1)) (t2 (first-term l2)) (o1 (order t1)) (o2 (order t2)))
          (cond
            ((> o1 o2) (adjoin-term t1 (add-terms (rest-terms l1) l2)))
            ((< o1 o2) (adjoin-term t2 (add-terms l1 (rest-terms l1))))
            (else
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
    (put 'mul (list t t) (lambda (x y) (tag (mul-poly x y))))
    (put '=zero? (list t)
      (lambda (p) (empty-termlist? (term-list p)))
    )
    (put 'make t (lambda (var term-list) (tag (make-poly var term-list))))
  )
  'done
)

(install-poly-package)
; (define (make-polynomial var term-list) ((get 'make 'polynomial) var term-list))

; (define ca (make-polynomial 'x '((2 2) (1 3) (0 1))))
; (define cb (make-polynomial 'x '((3 4) (1 1))))
; (define cc (make-polynomial 'x '((1 1) (0 10))))
; (define cd (make-polynomial 'x '((3 -4) (1 -1))))
; (define p1 (make-polynomial 'y (list (list 2 ca) 
;                                      (list 1 cb) 
;                                      (list 0 cc))))
; (define p2 (make-polynomial 'y (list (list 1 ca) 
;                                      (list 0 cb)))) 
; (define p3 (make-polynomial 'y (list (list 2 ca) 
;                                      (list 1 cd) 
;                                      (list 0 cc))))
; (display (add ca cb))(newline)
; (display (=zero? (add cb cd)))(newline)
; (display (mul ca cc))(newline)
