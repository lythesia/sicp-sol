;; in database we have:
;; (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
;; (supervisor (Fect Cy D) (Bitdiddle Ben))
;; (supervisor (Tweakit Lem E) (Bitdiddle Ben))
;; (supervisor (Cratchet Robert) (Scrooge Eben))
;; and about: Oliver
;; (supervisor (Bitdiddle Ben) (Warbucks Oliver))
;; (supervisor (Scrooge Eben) (Warbucks Oliver))

;; wheel rule:
;; (rule (wheel ?person)
;;   (and
;;     (supervisor ?middle ?person) ; p1
;;     (supervisor ?x ?middle)      ; p2
;;   )
;; )

;; when scan all assertions with p1, we get all supervisor assertions as stream,
;; then for each in stream, with ?middle binded with some specific person, apply
;; p2 produces: Bitdiddle has 3 assignment match it, here gives Oliver 3 times,
;; and Scrooge 1.
