; a)
(and
  (supervisor ?person (Ben Bitdiddle))
  (address ?person ?address)
)

; b)
(and
  (sallary (Ben Bitdiddle) ?sallary1)
  (sallary ?person ?sallary2)
  (lisp-value > ?sallary1 ?sallary2)
)

; c)
(and
  (supervisor ?person ?boss)
  (not (job ?boss (computer . ?type)))
  (job ?person ?work) ; <- i think here should be ?person, not ?boss(meteorgan)
)
