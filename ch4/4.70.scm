;; `THE-ASSERTIONS` in `(cons-stream assertion THE-ASSERTIONS)` is not even evaluated(即便作为实际参数, 他仍然
;; 没有被 eval), so when this part gets evaluated acturally(e.g: in some stream-cdr), it finds itself
;; `(cons-stream assertion THE-ASSERTIONS)`, in stream-car it produce assertion, and in stream-cdr it once again
;; find itself cons-steam, which is inf loop.
;;
;; just like (define ones (cons-stream 1 ones)), it's an inf repeat stream.
