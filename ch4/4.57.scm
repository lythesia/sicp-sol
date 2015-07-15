(rule (replace ?person1 ?person2)
  (and
    (job ?person1 ?job1)
    (job ?person2 ?job2)
    (or
      (same ?job1 ?job2)
      (can-do-job ?job1 ?job2)
    )
    (not (same ?person1 ?person2))
  )
)

; a)
(replace ?person (Cy D.Fect))

; b)
(and
  (salary ?person1 ?amount1)
  (salary ?person2 ?amount2)
  (list-value > ?amount1 ?amount2)
  (replace ?person1 ?person2)
)
