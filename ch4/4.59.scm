; a)
(meeting ?dept (Friday . ?time))

; b)
(rule
  (meeting-time ?person ?day-and-time)
  (or
    (meeting whole-company ?day-and-time)
    (and
      (job ?person (?dept . ?job))
      (meeting ?dept ?day-and-time)
    )
  )
)

; c)
(meeting-time Alyssa (Wednesdya . ?time))
