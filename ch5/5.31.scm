(f 'x 'y)     ; no need, it's trivial
((f) 'x 'y)   ; (f) may change env, but 'x 'y both dont need env, so all no need
(f (g 'x) y)  ; (g 'x) need proc argl and may change env, so all need
(f (g 'x) 'y) ; though (g 'x) may change env, but 'y dont need env
