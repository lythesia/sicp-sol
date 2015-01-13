(load "3.05.01.stream.scm")
(load "3.50.scm") ; generic map

(define sum 0)
(define (acc-sum x) (set! sum (+ sum x)) sum)
(define (disp-sum) (display "sum: ") (display sum) (newline))

(define seq (stream-map acc-sum (stream-enum-interval 1 20))) ; 1 ..
(disp-sum) ; single: 1; seq: 1

;; comment-out each to check
;; or seq
; (define y (stream-filter even? seq))
; (disp-sum) ; single: 6; seq: 6

(define z (stream-filter (lambda (x) (zero? (remainder x 5))) seq)) ; [1 3 6] 10 ..
(disp-sum) ; single: 10; seq: 15 (2,3 repeated)

; (stream-ref y 7)
; (disp-sum)  ; single: 136;

(display-stream z)
(newline)
(disp-sum)  ; single: 210

(display-stream z)
(newline)
(disp-sum)  ; seq: 410 = 210 + 200 (5-20 repeated)
