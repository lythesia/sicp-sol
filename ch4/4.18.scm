(define (solve f y0 dt)
  (let ((y '*unassigned*) (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b)
    )
    y
  )
)

;; convert let -> lambda
(define (solve f y0 dt)
  (let ((y '*unassigned*) (dy '*unassigned*))
    ((lambda (a b)  ; 1. define lambda
       (set! y a)
       (set! dy b)
     )
     (integral (delay dy) y0 dt)  ; 2. invoke lambda
     (stream-map f y)
    )
    y
  )
)


;; 根据具体实现不同, 实际参数 eval 的顺序也不同, 从左到右的话就会先 eval a, 即计算 (integral (delay dy) y0 dt),
;; 而此时 dy 是未赋值的, 但由于 delay 的作用, 此时 dy 的未赋值状态是被无视的, 接着 eval b, 此时 y 是未赋值并且被
;; 立即使用了(delay 并没有真正"使用"), 所以报错.
;; 而从右往左的话则是先 eval b, 并报错.
;;
;; 为何正文的版本 work?
;; 解释过程同上.
;; 可以参考这个解释: [4.18](http://d.hatena.ne.jp/tmurata/20100325/1269520114)
