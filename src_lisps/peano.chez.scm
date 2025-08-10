(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "list-display.scm")

(define appendo (lambda (xs ys xys)
  (conde
    ((== xs '()) (== ys xys))
    ((fresh (h tl temp)
      (== xs `(,h . ,tl))
      (== xys `(,h . ,temp))
      (appendo tl ys temp)
      ))
  )))

(define addo (lambda (x y xy)
  (conde
    [(== x 'z) (== y xy)]
    [(fresh (p tmp)
        (== x `(s ,p))
        (== xy `(s ,tmp))
        (addo p y tmp))
        ]
    )))

(list-display
  (run* (q)
    (addo '(s (s z)) '(s (s z)) q )))
(list-display
  (run* (q)
    (addo '(s (s z)) q '(s (s (s (s z)))) )))

(display "Test disequality\n")
(list-display
  (run* (q)
    (fresh (p)
        (=/= q '(s (s (s p))))
        (==  q '(s (s (s (s z))))) )))
