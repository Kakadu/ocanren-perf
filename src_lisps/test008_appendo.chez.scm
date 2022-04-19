(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")
(include "list-display.scm")

(display "============================= appendo test\n")

(define == ==count)
(define appendo11 (lambda (a b ab)
  (conde
    ( (== '() a )
      (== b ab  )
    )
    ((fresh (h tl temp)
        (== a `(,h . ,tl) )
        (== ab `(,h . ,temp) )
        (appendo11 tl b temp)
        )) )))

(define do_measure (lambda ()
  (run 100 (q r)
    (appendo11 q r (makelist 20) ))
  ;(run 3 (q r)
  ;  (appendo11 '(1 2 3) q r ))
))


(if (not (getenv "BENCH_MODE"))
  (begin
    (list-display (do_measure))
    (report_counters)
    (exit)
      ))
