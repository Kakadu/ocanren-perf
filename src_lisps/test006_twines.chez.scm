(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

; twines
(list-display
  (run 15 (x)
    (fresh (p q)
      (=/= p q)
      (eval-expo p '() `(val_ ,q))
      (eval-expo q '() `(val_ ,p))
      (== `(,p ,q) x)))
)

;(printf "unif-counter = ~a\n" unif-counter)
