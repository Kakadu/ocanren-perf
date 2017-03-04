#lang racket
(require racket/include)

(require "../faster-miniKanren/mk.rkt")
(include "../faster-miniKanren/test-check.scm")

(include "q.scm")

; thrines
;(loop for x in answers
;         do (print x) )

;(length answers)
(define answers
  (run 5 (x)
    (fresh (p q r)
      (=/= p q)
      (=/= q r)
      (=/= r p)
      (eval-expo p '() `(val_ q))
      (eval-expo q '() `(val_ r))
      (eval-expo r '() `(val_ p))
      (== `(,p ,q ,r) x))) )

(map println answers)

;(dump-memory-stats)
