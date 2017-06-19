(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "list-display.scm")
(include "q_nodiseq.scm")

(define do_measure (lambda ()
  (run 1 (q)
    (ev '()
        q
        `(code ,q)
    ))
))

(if (not (getenv "DONT_RUN_CHEZ"))
  (begin
    (list-display (do_measure))
    (report_counters)
    (exit)
      ))
