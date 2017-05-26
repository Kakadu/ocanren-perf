(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
;(include "../simple-miniKanren/test-check.scm")
(include "list-display.scm")


(define myappendo (lambda (a b ab)
    (conde
      ( (=== a '())
        (=== b ab)  )
      ((fresh (h t res)
         (=== a  `(,h . ,t)   )
         (=== ab `(,h . ,res) )
         (myappendo t b res))) )))

(list-display
  (run 2 (q )
    (myappendo q '() q ))
    )

(report_counters)
