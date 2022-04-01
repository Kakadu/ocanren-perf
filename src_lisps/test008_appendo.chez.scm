(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
;(include "../simple-miniKanren/test-check.scm")
(include "list-display.scm")

(display "============================= appendo test\n")

(define appendo11 (lambda (a b ab)
  (conde
    ( (trace "appendo") fail)
    ( (== '() a)
      (trace "a")
      (== b ab)
      (trace "e")
    )
    ((fresh (h tl temp)
        (trace "b")
        (== a `(,h . ,tl) )
        (== ab `(,h . ,temp))
        (trace "c")
        (appendo11 tl b temp)
        (trace "d")
        )) )))

(define do_measure (lambda ()
  (run 3 (q r)
    (appendo11 q r '(1 2 3) ))
))


(if (not (getenv "BENCH_MODE"))
  (begin
    (list-display (do_measure))
    (report_counters)
    (exit)
      ))
