(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

;(include "q.scm")
(include "list-display.scm")

(list-display
  (myrun1 1 (q)
    (conde
      ((=== 1 2))
      ((=== 3 4))
      ((=== 5 6) (=== 7 8))
    )
))

(report_counters)
