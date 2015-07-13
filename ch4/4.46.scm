;; take `(parse-sentence (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))` for example:
;; our `parse-word` proc takes token from `*unprased*` from left to right, if we evalutes right
;; first, here the `parse-verb-phrase`, the it will fetch a noun-phrase token as verb token,
;; which is wrong
