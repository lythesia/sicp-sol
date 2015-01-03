(define (make-table)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond
        ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))
      )
    )

    (define (lookup key-list)
      (define (lookup-aux keys table)
        (let ((sub-table (assoc (car keys) (cdr table))))
          (if sub-table
            (if (null? (cdr keys))
              (cdr sub-table)
              (lookup-aux (cdr keys) sub-table)
            )
            #f
          )
        )
      )
      (lookup-aux key-list local-table)
    )

    (define (insert! key-list val)
      (define (make-entry keys)
        (if (null? (cdr keys))
          (cons (car keys) val)
          (list (car keys) (make-entry (cdr keys)))
          ; (cons .. (make-entry ..)) is wrong, since each entry is one elem in a list whose leader is 'symbol
        )
      )
      (define (insert-aux! keys table)
        (let ((sub-table (assoc (car keys) (cdr table))))
          (if sub-table
            ; exist
            (if (null? (cdr keys))
              (set-cdr! sub-table val)
              (insert-aux! (cdr keys) sub-table)
            )
            ; not exist
            (set-cdr! table (cons (make-entry keys) (cdr table)))
          )
        )
      )
      (insert-aux! key-list local-table)
      'ok
    )

    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown operation -- TABLE" m))
      )
    )
    dispatch
  )
)

; test
; 1-d table
; (define table1 (make-table))
; (define get1 (table1 'lookup-proc))
; (define put1 (table1 'insert-proc!))
; (put1 '(a) 2.0)
; (display (get1 '(a)))(newline) ;Value: 2.
; (put1 '(b) 3)
; (put1 '(c) 4)
; (put1 '(a) 1)
; (display (get1 '(a)))(newline) ;Value: 1
; (display (get1 '(b)))(newline) ;Value: 3
; (display (get1 '(c)))(newline) ;Value: 4
; (newline)

; ; 2-d table
; (define table2 (make-table))
; (define get2 (table2 'lookup-proc))
; (define put2 (table2 'insert-proc!))
; (put2 '(a x) 11)
; (put2 '(b x) 21)
; (put2 '(b y) 22)
; (put2 '(a y) 12)
; (display (get2 '(a x)))(newline) ;Value: 11
; (display (get2 '(a y)))(newline) ;Value: 12
; (display (get2 '(b x)))(newline) ;Value: 21
; (display (get2 '(b y)))(newline) ;Value: 22
; (display (get2 '(a b)))(newline) ;Value: #f
; (put2 '(b x) 100)
; (display (get2 '(b x)))(newline) ;Value: 100
; (newline)

; ; 4-d table
; (define fermions (make-table))
; (define getf (fermions 'lookup-proc))
; (define putf (fermions 'insert-proc!))
; (putf '(quarks particles top mass) '(2.4 MeV))
; (putf '(quarks particles top charge) 2/3)
; (putf '(quarks particles top spin) 1/2)
; (putf '(quarks antiparticles top mass) '(2.4 MeV))
; (putf '(quarks antiparticles top charge) -2/3)
; (putf '(quarks antiparticles top spin) 1/2)
; (putf '(leptons particles electron mass) '(0.511 Mev))
; (putf '(leptons particles electron charge) -1)
; (putf '(leptons particles electron spin) 1/2)
; (putf '(leptons particles muon mass) '(105.7 Mev))
; (putf '(leptons particles muon charge) -1)
; (putf '(leptons particles muon spin) 1/2)
; (display (getf '(quarks particles top mass)))(newline) ;Value: (2.4 mev)
; (display (getf '(quarks particles top charge)))(newline) ;Value: 2/3
; (display (getf '(quarks particles top spin)))(newline) ;Value: 1/2
; (display (getf '(quarks antiparticles top mass)))(newline) ;Value: (2.4 mev)
; (display (getf '(quarks antiparticles top charge)))(newline) ;Value: -2/3
; (display (getf '(quarks antiparticles top spin)))(newline) ;Value: 1/2
; (display (getf '(leptons particles electron mass)))(newline) ;Value: (.511 mev)
; (display (getf '(leptons particles electron charge)))(newline) ;Value: -1
; (display (getf '(leptons particles electron spin)))(newline) ;Value: 1/2
; (display (getf '(leptons particles muon mass)))(newline) ;Value: (105.7 mev)
; (display (getf '(leptons particles muon charge)))(newline) ;Value: -1
; (display (getf '(leptons particles muon spin)))(newline) ;Value: 1/2
; (putf '(leptons particles muon spin) 1)
; (display (getf '(leptons particles muon spin)))(newline) ;Value: 1 
; (putf '(leptons particles muon spin) 1/2)
; (display (getf '(leptons particles muon spin)))(newline) ;Value: 1/2
