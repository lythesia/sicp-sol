;; As I noted in textbook, if the operator is result of some other compound 
;; procedures, then eval will give a "delayed" one, which is not valid proc
;; as `(list 'procedure)` or `(list 'primitive)`, that cannot be applied.
