(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

; quines
(define do_measure (lambda ()
  (run 200 (p)
    (eval-expo p '() `(val_ ,p)) )
))

(if (not (getenv "BENCH_MODE"))
  (begin
    (list-display (do_measure))
    (report_counters) ; 274068 unification for first 200 quines
      ; 2085 for first quine
    (exit)
      ))
