;; when apply 2nd part of `or`, (outranked-by ?moddle-manager ?boss) will query outranked-by again, both param
;; are not constrained, so it will endlessly call.
