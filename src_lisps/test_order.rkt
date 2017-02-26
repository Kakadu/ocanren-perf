#lang racket
(require racket/include)

(require "../faster-miniKanren/mk.rkt")
;(include "../faster-miniKanren/test-check.scm")

(run* (q)
    (conde
      ((== `1 q))
      ((== `2 q))
      ((== `3 q))
      ((== `4 q))
      ((== `5 q))
       ))
