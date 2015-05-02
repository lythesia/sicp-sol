;              +---------------------------------------------------+
;              |                                                   |
;   base env   |   lambda_any                                      |
;              |       |                                           |
;              +-------+-------------------------------------------+
;                      |                       ^
;                      |                       |
;                      |                       |
;                      |             +---------+--------+
;                      |             | u: '*unassigned* |
;                      |             | v: '*unassigned* |
;                      |             +------------------+
;                      |                       ^
;                      |                       |
;                      +-------->[*][*]--------+
;                                 |
;                                 v
;                               param: vars
;                               body: (set! u <e1>)
;                                     (set! v <e2>)
;                                     <e3>

;; since `let` used, which actually act as lambda, as:
;; let ((x <e1> ..)) will be converted into ((lambda (x ..) <body>) <e1>)
;;
;; think about why we need `scan-out-defines`(in 4.15.scm)? since until we parse the exprs thoroughly, we cannot
;; use it. e.g: in some closure, we define procedure u and then v, both invoke another recursively, when we parse
;; to u, it calls v, but for now we do not know what v is(exactly, we have not bind v to current env).
;; one solution is to **LAZY it: we wrap all internal defines with initial `'*unassigned*`, and in its body we give
;; then definitions via `set!`, though we don't care what is `set!` to them, we leave it to real `apply` process,
;; during which it starts to eval the definitions.
;; 
;; see the whole eval -> apply process:
;;
;; eval top-define
;;       |
;; eval define-value(body)
;;       |
;;        \
;;        eval sub-define in closure
;;              |
;;        eval sub-define-value(body) (here makes lambda for converting defines, when it'evaled, make-procedure is called respect to the generated lambda) (1)
;;              |
;;        eval: found application (call another define which is not evaluted yet since it's the following context) 
;;              |
;;        apply that define with var in current env
;;              |
;;               \
;;              eval the operator(the name of define of procedure to be called) within env
;;                    |
;;              variable? yes, lookup it: oops, "Unbound variable", why? we have not define it (or say not yet 
;;              bound to env; when will it add to env? after it's definition is evaluated)!
;;
;; what if with the let wrapper?
;; since we install `scan-out-defines` in make-procedure, after (1), we use `let` defines these names! then eval 
;; rest sequence, note `set!`'s value is now evaluated, in this case generate a make-procedure, and this proc is
;; bound to env! So in application afterwards, it can be lookup successfully.
;; 
;; another solutions is to rearrange all defines to beginning of procedure body.
