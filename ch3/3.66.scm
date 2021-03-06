; a)
; stream-append:
;   S0 will consume up all in T, thus S1, S2 .. prefix will not generate

; b)
; interleave
; 1,1 ---- 1,2 ---- 1,3 ---- 1,4 .. ----> as 1st stream
;      `-- 2,2 ---- 2,3 ---- 2,4 .. \
;               `-- 3,3 ---- 3,4 ..  +--> as 2nd stream (recursively)
;                        `-- 4,4 .. /
; generate interleave-wise:
; 1,1 1,2 2,2 1,3 2,3 1,4 3,3 1,5 2,4 1,6 3,4 ...
; 
; ===================================================
;     1   2   3   4   5   6   7   8   9  ...  100   
; ---------------------------------------------------
; 1   1   2   4   6   8  10  12  14  16       198 
; 2       3   5   9  13  17  21  25  29        
; 3           7  11  19  27  35  43  51
; 4              15  23  39  .....
; 5                  31  .........
; .
; .
; 100 ======================================= (2^100 - 1)
;
; 1. f(n, n) = 2^n - 1
; 2. f(n, n+1) = f(n+1, n+1) - 2^(n-1) = 2^(n+1) -1 - 2^(n-1) = 2^n + 2^n - 2^(n-1) - 1 = (2^n - 1) + 2^(n-1)
; 3. f(n, m > n+1) = f(n, n) + 2^(n-1) + (m - n - 1)*2^n = (2^n - 1) + 2^(n-1) + (m-n-1)*2^n ( 2. = 3. )
; in total: f(n, m) = 2^(n-1) * (max{1, 2*(m-n)} + 1) - 1
;                 +-- 1. 2^(n-1) * 2 -1
;                 `-- 2. 2^(n-1) * 2(m-n) + 2^(n-1) - 1
