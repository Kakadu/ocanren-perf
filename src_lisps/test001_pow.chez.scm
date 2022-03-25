(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "../faster-miniKanren/test-check.scm")
(include "list-display.scm")

(define poso
  (lambda (n)
    (fresh (a d)
      (== `(,a . ,d) n))))

(include "numbers.scm")

(define do_measure (lambda ()
  (run* (q)
    ;(expo '(1 1) '(1) q)))

    (expo '(1 1) '(1 0 1) q)) ; 3^5
))

(if (not (getenv "BENCH_MODE"))
  (begin
    (list-display (do_measure))
    (report_counters)
    (exit)
  ))
