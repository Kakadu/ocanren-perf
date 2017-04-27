(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

(list-display
  (run* (exp)
    (conde
      ((fresh (t1)
          (=== exp 0)
          ;(conde
          ;  ((=== t1 1) );(=== t1 2))
          ;)
          ))
      ((fresh (t2)
          (=== exp 3)
          ;(conde
          ;  ((=== t2 4) );(=== t2 5))
          ;)
          ))
      ((fresh (t3)
          (=== exp 6)
          ;(conde
          ;  ((=== t3 7) );(=== t3 8))
          ;)
          ))
    )
  )
)
