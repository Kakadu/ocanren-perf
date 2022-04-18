(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

(define count 200)
; quines
(define do_measure (lambda ()
  (run count (p)
    (eval-expo p '() `(val_ ,p)) )
))

(if (not (getenv "BENCH_MODE"))
  (begin
    (list-display (do_measure))
    (report_counters) ; 274068 unification for first 200 quines
      ; 2085 for first quine
      ; 6920
      ; 8480

      ; 18798 for 10th

      ; 274068 for 200th
    (exit)
      ))
