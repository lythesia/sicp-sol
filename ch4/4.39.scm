;; final answer will not be affected, but run time will be.
;;
;; we can see that the implementation of `distinct?` is O(m) not O(1), so it's an expensive invoke.
;; generally we want to delay the calls of expensive requirements, and we want to call the more restrictive
;; requirements earlier. 
;;
;; here is code snip for reference, which place the expensive `distinct?` at end:
;; # python2
;; def distinct(*args):
;;  return len(set(args)) == len(args)
;;  
;;  from itertools import product
;;  for i in range(1000):
;;    for b, c, f, m, s in product([1, 2, 3, 4 ,5], repeat=5):
;;      # if not distinct(b, c, f, m, s): continue # slower
;;      if b == 5: continue
;;      if c == 1: continue
;;      if f == 5: continue
;;      if f == 1: continue
;;      if m <= c: continue
;;      if abs(s - f) == 1: continue
;;      if abs(f - c) == 1: continue
;;      if not distinct(b, c, f, m, s): continue # faster
;;      print [b, c, f, m, s]
;;
;; and you can do quantity estimation! (5^5 .. 5! .. 4 * 5^4)
