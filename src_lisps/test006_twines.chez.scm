(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

; twines
(define do_measure (lambda ()
  (run 15 (x)
    (fresh (p q)
      (=/= p q)
      (eval-expo p '() `(val_ ,q))
      (eval-expo q '() `(val_ ,p))
      (== `(,p ,q) x)))
))

(if (not (getenv "DONT_RUN_CHEZ"))
  (begin
    (list-display (do_measure))
    (report_counters)
    (exit)
  ))
