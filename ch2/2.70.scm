(load "2.68.scm")
(load "2.69.scm")

(define pairs 
  '(
    (A 2)
    (BOOM 1)
    (NA 16)
    (SHA 3)
    (GET 2)
    (JOB 2)
    (YIP 9)
    (WAH 1)
  )
)

(define tree (generate-huffman-tree pairs))
(define msg 
  '(
    GET A JOB
    SHA NA  NA  NA  NA  NA  NA  NA  NA
    GET A JOB
    SHA NA  NA  NA  NA  NA  NA  NA  NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM
  )
)
(define encoded (encode msg tree))
(display (encode '(GET A JOB) tree))(newline)
(display (encode '(SHA NA  NA  NA  NA  NA  NA  NA  NA) tree))(newline)
(display (encode '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP) tree))(newline)
(display (encode '(SHA BOOM) tree))(newline)

(display (length (encode '(GET A JOB) tree)))(newline)
(display (length (encode '(SHA NA  NA  NA  NA  NA  NA  NA  NA) tree)))(newline)
(display (length (encode '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP) tree)))(newline)
(display (length (encode '(SHA BOOM) tree)))(newline)
(display (length encoded))(newline)
(display encoded)(newline)
