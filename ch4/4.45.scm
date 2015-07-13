;; 1) prof + class + cat
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb-phrase
        (verb lectures)
        (prep-phrase
          (prep to)
          (simple-noun-phrase (article the) (noun student))
        )
      )
      (prep-phrase
        (prep in)
        (simple-noun-phrase (article the) (noun class))
      )
    )
    (prep-phrase
      (prep with)
      (simple-noun-phrase (article the) (noun cat))
    )
  )
)

;; 2) prof + cat, stu + class
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb lectures)
      (prep-phrase
        (prep to)
        (noun-phrase
          (simple-noun-phrase (article the) (noun student))
          (prep-phrase
            (prep in)
            (simple-noun-phrase (article the) (noun class))
          )
        )
      )
    )
    (prep-phrase
      (prep with)
      (simple-noun-phrase (article the) (noun cat))
    )
  )
)

;; 3) prof + class, class + cat
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb lectures)
      (prep-phrase
        (prep to)
        (simple-noun-phrase (article the) (noun student))
      )
    )
    (prep-phrase
      (prep in)
      (noun-phrase
        (simple-noun-phrase (article the) (noun class))
        (prep-phrase
          (prep with)
          (simple-noun-phrase (article the) (noun cat))
        )
      )
    )
  )
)

;; 4) stu + class, class + cat
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb lectures)
    (prep-phrase
      (prep to)
      (noun-phrase
        (simple-noun-phrase (article the) (noun student))
        (prep-phrase
          (prep in)
          (noun-phrase
            (simple-noun-phrase (article the) (noun class))
            (prep-phrase
              (prep with)
              (simple-noun-phrase (article the) (noun cat))
            )
          )
        )
      )
    )
  )
)

;; 5) stu + class + cat
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb lectures)
    (prep-phrase
      (prep to)
      (noun-phrase
        (noun-phrase
          (simple-noun-phrase (article the) (noun student))
          (prep-phrase
            (prep in)
            (simple-noun-phrase (article the) (noun class))
          )
        )
        (prep-phrase
          (prep with)
          (simple-noun-phrase (article the) (noun cat))
        )
      )
    )
  )
)
