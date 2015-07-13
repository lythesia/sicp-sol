;; a)
;; take "the professor lecturs" for example:
;; amb 1st time has 2 options:
;; 1. (verb lectures)
;; 2. (verb-phrase <parse-verb-phrase> <parse-prepositional-phrase>)
;; note the middle `<parse-verb-phrase>`, it also has 2 options:
;; 2.1. pure verb
;; 2.2. verb + prep
;; it's ok to choose 2.1, then it's time for `<parse-prepositional-phrase>`, but it will fail, the amb will try
;; 2.2, and lead to inf loop.

;; b)
;; nope.
