(define (parse-word word-list)
  (list (car word-list) (apply amb (cdr word-list)))
)
