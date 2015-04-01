(define input-prompt ";;; M-EVAL input:")
(define output-prompt ";;; M-EVAL output:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read))) ; read wait for input
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)
    )
  )
  (driver-loop)
)

(define (prompt-for-input string)
  (newline)(newline)
  (display string)
  (newline)
)
(define (announce-output string)
  (newline)
  (display string)
  (newline)
)
(define (user-print object)
  (if (compound-procedure? object)
    (display
      (list
        'compound-procedure           ; check 4.01.03
        (procedure-parameters object)
        (procedure-body object)
        '<procedure-env>
      )
    )
    (display object)
  )
)
