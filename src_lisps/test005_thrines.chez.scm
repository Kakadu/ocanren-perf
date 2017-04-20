(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

(list-display
  (run 1
    ;(p q r)
    (x)
    (fresh (p q r)
    ;((bind*
      ;(=/= p q)
      ;(=/= q r)
      ;(=/= r p)
      (eval-expo p '() `(val_ ,q))
      ;(eval-expo q '() `(val_ ,r))
      ;(eval-expo r '() `(val_ ,p))
      (== `(,p ,q ,r) x)
      ))
)
