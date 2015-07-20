;; second arg `(flatten-stream (stream-cdr stream))` will be evaluated, which will stream-car the (stream-cdr stream),
;; and it recursively will call flatten, as result `stream` will always be stream-car ed, so it will stun if `stream`
;; is inf stream.
