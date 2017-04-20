;(include "../simple-miniKanren/mk.scm")
(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "list-display.scm")
;(include "q_nodiseq.scm")

(define ===
  (lambda (x y)
    (lambda (s)
      (let ((x2 (walk x s))
            (y2 (walk y s)))
        (printf "unify '~a' and '~a'\n" x2 y2)
        ((== x y) s) ))))

(define custom1 (lambda (q)
  (=== q 2)
  ))

(list-display
  (run 2 (q)
    (conde
      ( (fresh (x)
          (=== q 1)
          (custom1 x)))
      ( (fresh (x y)
            (=== q 4)
            (=== x 5)
            (=== y 6)))
        ))
)
