(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "../faster-miniKanren/test-check.scm")

(define poso
  (lambda (n)
    (fresh (a d)
      (== `(,a . ,d) n))))

(include "numbers.scm")


; without display REPL prints the result but compiled code doesn't
(map (lambda (x) (printf "~a\n" x))
  (run* (q)
    (logo '(1 1 0 0 1 1 1 1) '(1 1) q '() )))
