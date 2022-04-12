(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "list-display.scm")
(include "q_nodiseq.scm")

; debugging order of quines without diseq constraints
(define do_measure (lambda ()
  (run 1 (q)
    (ev '()
        q
        `(code ,q)
    ))
))

(list-display (do_measure))
(report_counters)
(exit)
