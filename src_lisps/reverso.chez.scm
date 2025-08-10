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

(define (reverso1 xy yx)
  (conde
    ((== xy '()) (== yx '()))
    ((fresh (h tl tmp)
      (== xy `(,h . ,tl))
      (appendo tmp `(,h) yx)
      (reverso1 tl tmp)
      ))
  ))

(list-display
  (run* (q)
    (appendo '(1 2) '(3 4) q)))
(list-display
  (run 1 (q)
    (reverso1 '(1 2 3 4) q)))