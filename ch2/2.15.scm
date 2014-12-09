; 1. R1R2 / (R1 + R2)  (wrong)
; 2. 1 / (1/R1 + 1/R2) (ok)
;
; Alyssa’s system will produce big errors when computing expressions 
; such as A/A. The reason is the following. When an interval A appears 
; twice in an expression, they should not behave independently. They are
; correlated. In fact, they are the same unit in a system. So if the 
; real value of A is x - dx, both A‘s should have the same value in a 
; good system. However, Alyssa’s program treats them independently. In 
; Alyssa’s system, one A may have a value of x - dx, whereas the other 
; A may have a value of x + dx. Therefore we conclude that Alyssa’s 
; system is flawed.
