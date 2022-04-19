(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "list-display.scm")

(define === (lambda (x y) (lambda (s)
  ((== x y) s)
)))

(define =//= (lambda (x y) (lambda (s)
  ((=/= x y) s)
)))

(include "q_nodiseq.scm")

(define do_measure (lambda ()
  (run 200 (q)
    (ev '()
        q
        `(code ,q)
    ))
))

(if (not (getenv "BENCH_MODE"))
  (begin
    (list-display (do_measure))
    (report_counters)
    (exit)
      ))
