(include "../simple-miniKanren/mk.scm")
(include "list-display.scm")

(include "q.scm")

; quines
(list-display
  (run 100 (p)
    (eval-expo p '() `(val_ ,p)) )
)
