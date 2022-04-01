(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "../faster-miniKanren/test-check.scm")
(include "list-display.scm")

(define lengtho (lambda (xs n)
  (conde
    ((== xs '()) (== n 'z))
    ((fresh (h tl prev)
      (== xs `(,h . ,tl))
      (== n `('s . ,prev))
      (lengtho tl prev)
      ))
  )))


(define lengtho2 (lambda (xs n)
  (conde
    ((== xs '()) (== n 'z))
    ((fresh (h tl prev)
      (== xs `(,h . ,tl))
      (lengtho tl prev)
      (== n `('s . ,prev))
      ))
  )))

(define appendo (lambda (xs ys xys)
  (conde
    ((== xs '()) (== ys xys))
    ((fresh (h tl temp)
      (== xs `(,h . ,tl))
      (== xys `('h . ,temp))
      (appendo tl ys temp)
      ))
  )))

(define appendo2 (lambda (xs ys xys)
  (conde
    ((== xs '()) (== ys xys))
    ((fresh (h tl temp)
      (== xs `(,h . ,tl))
      (appendo2 tl ys temp)
      (== xys `('h . ,temp))
      ))
  )))

(define input (make-list 10000 '1))

(define do_measure (lambda ()
  (run* (q)
    (appendo input '() q))
))

(if (not (getenv "BENCH_MODE"))
  (begin
    (list-display (do_measure))
    (report_counters)
    (exit)
  ))
