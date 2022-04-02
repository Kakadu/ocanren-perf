(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "list-display.scm")

(display "============================= appendo test\n")

(define appendo11 (lambda (a b ab)
  (conde
    ( (trace "appendo") fail)
    ( (== '() a 'A)
      (== b ab  'E)
    )
    ((fresh (h tl temp)
        (== a `(,h . ,tl) 'F)
        (== ab `(,h . ,temp) 'C)
        (appendo11 tl b temp)
        )) )))

(define do_measure (lambda ()
  ;(run 3 (q r)
  ;  (appendo11 q r '(1 2 3) ))
  (run 3 (q r)
    (appendo11 '(1 2 3) q r ))
))


(if (not (getenv "BENCH_MODE"))
  (begin
    (list-display (do_measure))
    (report_counters)
    (exit)
      ))
