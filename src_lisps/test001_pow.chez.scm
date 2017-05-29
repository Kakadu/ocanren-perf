(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "../faster-miniKanren/test-check.scm")
(include "list-display.scm")

(define poso
  (lambda (n)
    (fresh (a d)
      (== `(,a . ,d) n))))

(include "numbers.scm")


; without display REPL prints the result but compiled code doesn't
(display
  (run* (q)
    (expo '(1 1) '(1) q)))
    ;(expo '(1 1) '(1 0 1) q)))


(report_counters)
