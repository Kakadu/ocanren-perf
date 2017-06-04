(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

; quines
(list-display
  (run 1 (p)
    (eval-expo p '() `(val_ ,p)) )
)

;(report_counters)
