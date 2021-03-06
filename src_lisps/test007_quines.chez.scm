(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

; quines
(define do_measure (lambda ()
  (run 200 (p)
    (eval-expo p '() `(val_ ,p)) )
))

(if (not (getenv "DONT_RUN_CHEZ"))
  (begin
    (list-display (do_measure))
    (report_counters)
    (exit)
      ))
