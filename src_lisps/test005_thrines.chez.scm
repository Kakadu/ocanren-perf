(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

(define do_measure (lambda ()
  (run 2 (x)
    (fresh (p q r)
      (=//= p q)
      (=//= q r)
      (=//= r p)
      (eval-expo p '() `(val_ ,q))
      (eval-expo q '() `(val_ ,r))
      (eval-expo r '() `(val_ ,p))
      (== `(,p ,q ,r) x)))
))

(if (not (getenv "BENCH_MODE"))
  (begin
    (list-display (do_measure))
    (report_counters)
    ; Should give 224671 unifications
    (exit)
      ))
;(report_counters)
;
;(define evalo2 (lambda (exp)
;  (conde
;    ((fresh (t)
;      (=== exp `(seq ((symb 'quote) ,t)) )
;      (=== t 2 )
;    ))
;    ((fresh (eeeeeeeeeeees)
;      (succeed))
;    ))))
;
;(list-display (run 1 (exp)
;    (evalo2 exp)
;    ;(conde
;    ;  ((fresh (t)
;    ;    (=== exp `(seq ((symb 'quote) ,t)) )
;    ;  ))
;    ;  ((fresh (eeeeeeeeeeees)
;    ;    (succeed))
;    ;  ))
;))
