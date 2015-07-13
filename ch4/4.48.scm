;; assume we have adj dict
(define (parse-noun-with-adjective)
  (amb
    (parse-word nouns)
    (list
      'adjective-noun
      (parse-word adjectives)
      (parse-noun-with-adjective)
    )
  )
)

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase (parse-word articles) (parse-noun-with-adjective))
)
