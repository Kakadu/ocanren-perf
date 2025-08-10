(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "list-display.scm")

(define leo (lambda (a b)
  (conde
    [(== a 'z)]
    [(fresh (tmp)
        (== a 'z)
        (== b `(s ,tmp)))]
    [(fresh (a0)
        (== a `(s ,a0))
        (== b `(s ,b0))
        (leo a0 b0))]
    )))

; TODO:

