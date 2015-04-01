;; As for primitive map, it's applied as `apply-primitive-procedure`,
;; or say `apply-in-underlying-scheme(apply) <body> <arg>`, which will be translated as:
;; `apply map (list proc (list ...args))`
;; note that `proc` now is a TAGGED list (the new definition under `scheme' we implemented),
;; so it's not a callable procedure.
;;
;; While `map` we define in new `scheme' is a COMPOUND procedure, which will call eval-sequence -> eval.

;; hint!
;; This suggests that any procedure which takes other procedures as arguments has to be
;; implemented as a compound procedure rather than a primitive in the underlying Scheme.
