; assumption
; a0: 0
; a1: 1
; after propagate: resutl = 0, agenda: empty
;
; FIFO
; set a0: 1 --> agenda:
;                 3: 1 and 1 --> set-out 1
; set a1: 0 --> agenda:
;                 3: 1 and 1 --> set-out 1
;                    1 and 0 --> set-out 0
; exec up-down => result = 0
;
; FILO
; set a0: 1 --> agenda:
;                 3: 1 and 1 --> set-out 1
; set a1: 0 --> agenda:
;                 3: 1 and 0 --> set-out 0
;                    1 and 1 --> set-out 1
; exec up-down => result = 1
