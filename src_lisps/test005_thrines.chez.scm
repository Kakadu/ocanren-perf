(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

(list-display
  (run 3 (x)
    (fresh (p q r)
      (=/= p q)
      (=/= q r)
      (=/= r p)
      (eval-expo p '() `(val_ ,q))
      (eval-expo q '() `(val_ ,r))
      (eval-expo r '() `(val_ ,p))
      (== `(,p ,q ,r) x)))
)