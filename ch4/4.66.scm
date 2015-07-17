;; take 4.65's scenario:
;; (and (wheel ?who) (salary ?who ?amount)) will get Oliver's salary 4 times.
;; so before accumlate, he should filter the stream with something like `unique`.
