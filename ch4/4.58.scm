(rule (big-head ?person ?dept)
  (and
    (job ?person (?dept . ?job1))
    (job ?other (?dept . ?job2))
    (not (supervisor ?person ?other))
  )
)
