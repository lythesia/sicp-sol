;; complexity reduce from O(n^3) to O(n^2), but it will be slower for some trivial cases.
;; e.g: (3, 4, 5), 4.35 finishes check and produce it directly, but 4.37 computes hsq and
;; compare with ksq for extra, also sqrt is somewhat an inefficient proc against *.
