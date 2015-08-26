(define (tagged-list? exp sym)
  (if (pair? exp) (eq? (car exp) sym) #f)
)
