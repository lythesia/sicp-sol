;; for sake of delay inf loop, take `married` for example:
;; first we should know `strea-append` is procedure, so the to-be-append-arg will be evaluated,
;; where this arg is `(apply-rules ..)`, which will invoke `apply-a-rule` for each frame,
;; in `apply-a-rule` (since first stream-car will always execute this), it will execute
;; `(qeval (rule-body clean-rule) (singleton-stream unify-result))`, which lead ?x = some-name, ?y to
;; ?y, ?x = some-name in `married` as simple query, and this simple query will also trigger check:
;; 1. all assertions; 2. all rules(as append in stream-append), and when it checks rules, it will do
;; all these over again, and again.
;;
;; if delay used, we can get answers, but also inf, yet is better than stuning in qeval with nothing
;; output.
