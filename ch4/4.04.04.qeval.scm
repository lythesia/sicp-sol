(include "4.04.04.stream.scm")
(include "4.04.04.table.scm")

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond
      ((assertion-to-be-added? q)
       (add-rule-or-assertion! (add-assertion-body q))
       (newline)
       (display "Assertion added to data base.")
       (query-driver-loop)
      )
      (else
        (newline)
        (display output-prompt)
        (display-stream
          (stream-map
            (lambda (frame) (instantiate q frame (lambda (v f) (contract-question-mark v))))
            (qeval q (singleton-stream '()))
          )
        )
        (query-driver-loop)
      )
    )
  )
)

(define (contract-question-mark variable)
  (string->symbol
    (string-append "?"
      (if (number? (cadr variable))
        (string-append (symbol->string (caddr variable)) "-" (number->string (cadr variable))) ; (? (2 x)) -> "?x-2"
        (symbol->string (cadr variable))
      )
    )
  )
)

(define (prompt-for-input string)
  (newline)(newline)
  (display string)
  (newline)
)

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond
      ((var? exp)
       (let ((binding (binding-in-frame exp frame)))
         (if binding
           (copy (binding-value binding))
           (unbound-var-handler exp frame)
         )
       )
      )
      ((pair? exp)
       (cons (copy (car exp)) (copy (cdr exp)))
      )
      (else exp)
    )
  )
  (copy exp)
)

(define (qeval query frame-stream)  ; e.g: query => (job (? x) (? y))
  (let ((q-proc (get (type query) 'qeval))) ; (get 'job 'qeval) => nil, since it's not compound(like and, or, not ..)
    (if q-proc
      (q-proc (contents query) frame-stream)
      (simple-query query frame-stream) ; non-compound gets here
    )
  )
)

;; {{ parse
(define (tagged-list? exp sym)
  (if (pair? exp) (eq? (car exp) sym) #f)
)
(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))


(define (type exp)
  (if (pair? exp)
    (car exp)
    (error "Unknown expression TYPE" exp)
  )
)
(define (contents exp)
  (if (pair? exp)
    (cdr exp)
    (error "Unknown expression CONTENTS" exp)
  )
)

; assertion
(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!)
)
(define (add-assertion-body exp)
  (car (contents exp))
)

; rule
; and
(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

; or
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

; not
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement) (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule))
    '(always-true)
    (caddr rule)
  )
)

; conv
(define (query-syntax-process exp) (map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
  (cond
    ((pair? exp)
     (cons (map-over-symbols proc (car exp)) (map-over-symbols proc (cdr exp))) ; ok, it's recursive into element
    )
    ((symbol? exp) (proc exp))
    (else exp)
  )
)
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
      (list
        '?
        (string->symbol (substring chars 1 (string-length chars)))
      )
      symbol
    )
  )
)

; binding
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame) (assoc variable frame))
(define (extend variable value frame) (cons (make-binding variable value) frame))
(define (make-binding variable value) (cons variable value))
;; }}


;; {{ simple query
; check:
; 1. assertions
; 2. rules
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append-delayed
        (find-assertions query-pattern frame)     ; assertions
        (delay (apply-rules query-pattern frame)) ; rules
      )
    )
    frame-stream
  )
)

;; { assertion
; get asserions according to pattern
(define (find-assertions pattern frame)
  (stream-flatmap
    (lambda (datum)
      (check-an-assertion datum pattern frame)
    )
    (fetch-assertions pattern frame) ; avoid repeat iterator all assertions
  )
)

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
      the-empty-stream
      (singleton-stream match-result)
    )
  )
)

; get may-be-extend frame
(define (pattern-match pat dat frame)
  (cond
    ((eq? frame 'failed) 'failed)
    ((equal? pat dat) frame)
    ((var? pat) (extend-if-consistent pat dat frame))
    ((and (pair? pat) (pair? dat))
     (pattern-match (cdr pat) (cdr dat) (pattern-match (car pat) (car dat) frame))
    )
    (else 'failed)
  )
)

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
      (pattern-match (binding-value binding) dat frame) ; if alread bound, check if match
      (extend var dat frame)
    )
  )
)

(define THE-ASSERTIONS the-empty-stream)
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
    (get-indexed-assertions pattern)
    (get-all-assertions)
  )
)
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream)
)
(define (get-all-assertions) THE-ASSERTIONS)
;; end assertion }

;; { rule
(define (apply-rules pattern frame)
  (stream-flatmap
    (lambda (rule)
      (apply-a-rule rule pattern frame)
    )
    (fetch-rules pattern frame)
  )
)

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern (conclusion clean-rule) query-frame))) ; also give frame
      (if (eq? unify-result 'failed)
        the-empty-stream
        (qeval (rule-body clean-rule) (singleton-stream unify-result))
      )
    )
  )
)

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond
        ((var? exp) (make-new-variable exp rule-application-id))
        ((pair? exp) (cons (tree-walk (car exp)) (tree-walk (cdr exp))))
        (else exp)
      )
    )
    (tree-walk rule)
  )
)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter
)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))) ; (? (2  x))
)

; get unified may-be-extend frame
(define (unify-match p1 p2 frame)
  (cond
    ((eq? frame 'failed) 'failed)
    ((equal? p1 p2) frame)
    ((var? p1) (extend-if-possible p1 p2 frame))
    ((var? p2) (extend-if-possible p2 p1 frame))
    ((and (pair? p1) (pair? p2))
     (unify-match (cdr p1) (cdr p2) (unify-match (car p1) (car p2) frame))
    )
    (else 'failed)
  )
)

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond
      (binding (unify-match (binding-value binding) val frame)) ; if var has binding, unify with new-val
      ((var? val)
       (let ((binding (binding-in-frame val frame)))
         (if binding
           (unify-match var (binding-value binding) frame) ; if val has binding
           (extend var val frame)
         )
       )
      )
      ((depends-on? val var frame) 'failed)
      (else (extend var val frame))
    )
  )
)

; check var := val, if val has var in it, if so, reject it
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond
      ((var? e)
       (if (equal? var e)
         #t
         (let ((b (binding-in-frame e frame))) ; not var but compound pattern
           (if b (tree-walk (binding-value b)) #f) ; check it recursively
         )
       )
      )
      ((pair? e)
       (or (tree-walk (car e)) (tree-walk (cdr e)))
      )
      (else #f)
    )
  )
  (tree-walk exp)
)

(define THE-RULES the-empty-stream)
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
    (get-indexed-rules pattern)
    (get-all-rules)
  )
)
(define (get-indexed-rules pattern)
  (stream-append
    (get-stream (index-key-of pattern) 'rule-stream)
    (get-stream '? 'rule-stream)
  )
)
(define (get-all-rules) THE-RULES)
;; end rule }

;; { common
; get
(define (get-stream key1 key2) ; common
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)
  )
)

; set
(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
    (add-rule! assertion)
    (add-assertion! assertion)
  )
)
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS (stream-cons assertion old-assertions))
    'ok
  )
)
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (stream-cons rule old-rules))
    'ok
  )
)
(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
    (let ((key (index-key-of assertion)))
      (let ((current-assertion-stream (get-stream key 'assertion-stream)))
        (put key 'assertion-stream (stream-cons assertion current-assertion-stream))
      )
    )
  )
)
(define (store-rule-in-index rule) ; <key, 'rule-stream> => (rule [promise..])
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
      (let ((key (index-key-of pattern)))
        (let ((current-rule-stream (get-stream key 'rule-stream)))
          (put key 'rule-stream (stream-cons rule current-rule-stream))
        )
      )
    )
  )
)
(define (indexable? pat)
  (or
    (constant-symbol? (car pat))
    (var? (car pat))
  )
)
(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)
  )
)
(define (use-index? pat)
  (constant-symbol? (car pat))
)
;; }
;; }}


(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream
    (conjoin (rest-conjuncts conjuncts) (qeval (first-conjunct conjuncts) frame-stream))
  )
)

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
    the-empty-stream
    (interleave-delayed
      (qeval (first-disjunct disjuncts) frame-stream)
      (delay (disjoin (rest-disjuncts disjuncts) frame-stream))
    )
  )
)

(define (negate operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (stream-null? (qeval (negated-query operands) (singleton-stream frame)))
        (singleton-stream frame)
        the-empty-stream
      )
    )
    frame-stream
  )
)

(define (lisp-value call frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (execute (instantiate call frame (lambda (v f) (error "Unknown pat var -- LISP-VALUE" v))))
        (singleton-stream frame)
        the-empty-stream
      )
    )
    frame-stream
  )
)
(define (execute exp)
  (apply
    (eval (predicate exp)) ; (eval (...) user-initial-environment)
    (args exp)
  )
)

(define (always-true ignore frame-stream) frame-stream)

;; {{ env
; the-empty-stream is in stream.scm
(define rule-counter 0)
(define get '())
(define put '())
(define (initialize-data-base rules-and-assertions)
  (define (deal-out r-and-a rules assertions)
    (cond
      ((null? r-and-a)
       (set! THE-ASSERTIONS (list->stream assertions))
       (set! THE-RULES (list->stream rules)) ; 2. when no rule to cons, bind to THE-RULES
       'done
      )
      (else
        (let ((s (query-syntax-process (car r-and-a))))
          (cond
            ((rule? s)  ; s: list [rule (same (? x) (? x))]
             (store-rule-in-index s)
             (deal-out (cdr r-and-a) (cons s rules) assertions) ; 1. cons rules together
            )
            (else       ; s: list [job (some-name) (some-job)]
             (store-assertion-in-index s)
             (deal-out (cdr r-and-a) rules (cons s assertions))
            )
          )
        )
      )
    )
  )
  (let ((operation-table (make-table)))
    (set! get (operation-table 'lookup-proc))
    (set! put (operation-table 'insert-proc!))
  )
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true)
  (deal-out rules-and-assertions '() '())
)
;; }}

;; {{ init
(define microshaft-data-base
  '(
    ;; from section 4.4.1
    (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (job (Bitdiddle Ben) (computer wizard))
    (salary (Bitdiddle Ben) 60000)

    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job (Hacker Alyssa P) (computer programmer))
    (salary (Hacker Alyssa P) 40000)
    (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (job (Fect Cy D) (computer programmer))
    (salary (Fect Cy D) 35000)
    (supervisor (Fect Cy D) (Bitdiddle Ben))

    (address (Tweakit Lem E) (Boston (Bay State Road) 22))
    (job (Tweakit Lem E) (computer technician))
    (salary (Tweakit Lem E) 25000)
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))

    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (job (Reasoner Louis) (computer programmer trainee))
    (salary (Reasoner Louis) 30000)
    (supervisor (Reasoner Louis) (Hacker Alyssa P))

    (supervisor (Bitdiddle Ben) (Warbucks Oliver))

    (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (job (Warbucks Oliver) (administration big wheel))
    (salary (Warbucks Oliver) 150000)

    (address (Scrooge Eben) (Weston (Shady Lane) 10))
    (job (Scrooge Eben) (accounting chief accountant))
    (salary (Scrooge Eben) 75000)
    (supervisor (Scrooge Eben) (Warbucks Oliver))

    (address (Cratchet Robert) (Allston (N Harvard Street) 16))
    (job (Cratchet Robert) (accounting scrivener))
    (salary (Cratchet Robert) 18000)
    (supervisor (Cratchet Robert) (Scrooge Eben))

    (address (Aull DeWitt) (Slumerville (Onion Square) 5))
    (job (Aull DeWitt) (administration secretary))
    (salary (Aull DeWitt) 25000)
    (supervisor (Aull DeWitt) (Warbucks Oliver))

    (can-do-job (computer wizard) (computer programmer))
    (can-do-job (computer wizard) (computer technician))

    (can-do-job (computer programmer)
                (computer programmer trainee))

    (can-do-job (administration secretary)
                (administration big wheel))

    (rule (lives-near ?person-1 ?person-2)
          (and (address ?person-1 (?town . ?rest-1))
               (address ?person-2 (?town . ?rest-2))
               (not (same ?person-1 ?person-2))))

    (rule (same ?x ?x))

    (rule (wheel ?person)
          (and (supervisor ?middle-manager ?person)
               (supervisor ?x ?middle-manager)))

    (rule (outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss)
              (and (supervisor ?staff-person ?middle-manager)
                   (outranked-by ?middle-manager ?boss))))
  )
)

;; Do following to reinit the data base from microshaft-data-base
;; in Scheme (not in the query driver loop)
(initialize-data-base microshaft-data-base)
;; }}

(if (not (defined? 'dont-run-qeval))
  (query-driver-loop)
)
