(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(define poso
  (lambda (n)
    (fresh (a d)
      (== `(,a . ,d) n))))

(define === ==)
(define =//= =/=)

(include "list-display.scm")
(include "numbers.scm")

(define do_measure (lambda ()
  (run* (q)
    (expo '(1 1) '(1 0 1) q)) ; 3^5
))

(if (not (getenv "BENCH_MODE"))
  (begin
    (list-display (do_measure))
    (exit)
  ))
