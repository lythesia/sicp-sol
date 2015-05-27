(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value)
)

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1
  )
)

;; a) apply order
;;
;; (factorial 5) [1]=>
;; [1]: 
;; (unless (= 5 1)
;;  (* 5 (factorial (- 5 1))) [2] =>
;;  1
;; )
;; [2]: (factorial 4) =
;; (unless (= 4 1)
;;  (* 4 (factorial (- 4 1))) [3] =>
;;  1
;; )
;; [3]: (factorial 3) =
;; ..
;; [5]: (factorial 1) =
;; (unless (= 1 1)
;;  (* 1 (factorial (- 1 1))) [6*] => this will continue computation (factorial 0) -> (factorial -1).. endlessly
;;  1
;; )

;; b) regular order(so if here is an exact procedure or not? me prefer former)
;;
;; (factorial 5) [1] =>
;; [1]:
;; (unless (= 5 1)
;;  (* 5 (factorial (- 5 1)))
;;  1
;; )
;; (if (= 5 1) 1 [lazy .. (factorial 4)]) [2] =>
;; [2]: force (factorial 4)
;; ..
;; [5]: (factorial 1) =
;; (unless (= 1 1)
;;  (* 1 (factorial (-1 1)))
;;  1
;; )
;; (if (= 1 1) 1 [lazy .. (factorial 0)]) => this returns 1 with lazy part uncomputed
