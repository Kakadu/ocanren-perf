(include "../simple-miniKanren/mk.scm")
(include "list-display.scm")
(include "q_nodiseq.scm")

(list-display
  (run 3 (q)
    (ev '()
        q
        `(code ,q)))
)

;(list-display
;  (run 2 (x)
;    (fresh (p q r)
;      (=/= p q)
;      (=/= q r)
;      (=/= r p)
;      (eval-expo p '() `(val_ ,q))
;      (eval-expo q '() `(val_ ,r))
;      (eval-expo r '() `(val_ ,p))
;      (== `(,p ,q ,r) x)))
;)
