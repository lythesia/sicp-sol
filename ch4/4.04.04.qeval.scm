(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q query-syntax-process (read)))
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
)

(define (qeval query frame-stream)
  (let ((q-proc (get (type query) 'qeval)))
    (if q-proc
      (q-proc (contents query) frame-stream)
      (simple-query query frame-stream)
    )
  )
)

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
        ((pair? exp) (cons (tree-walk (car exp) (tree-walk (cdr exp)))))
        (else exp)
      )
    )
    (tree-walk rule)
  )
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
           (if b (tree-walk (binding-value b) #f)) ; check it recursively
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
    (set! THE-ASSERTIONS (cons-stream assertion old-assertions))
    'ok
  )
)
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok
  )
)
(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
    (let ((key (index-key-of assertion)))
      (let ((current-assertion-stream (get-stream key 'assertion-stream)))
        (put key 'assertion-stream (cons-stream assertion current-assertion-stream))
      )
    )
  )
)
(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
      (let ((key (index-key-of pattern)))
        (let ((current-rule-stream (get-stream key 'rule-stream)))
          (put key 'rule-stream (cons-stream rule current-rule-stream))
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
(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
  (if (empty-disjuncts? disjuncts)
    the-empty-stream
    (interleave-delayed
      (qeval (first-disjunct disjuncts) frame-stream)
      (delay (disjoin (rest-disjuncts disjuncts) frame-stream))
    )
  )
)
(put 'or 'qeval disjoin)

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
(put 'not 'qeval negate)

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
(put 'list-value 'qeval list-value)
(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment) (args exp))
)

(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)
