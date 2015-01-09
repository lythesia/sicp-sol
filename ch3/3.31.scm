; remove (proc) cause the action cannot immediately add to agenda
; you need to call `set-signal!` manually to do that
; but the `set-signal!` needs to be changed: it should re-bind actions whenever called(even though wire value not changed!)
