(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")


;; TODO: just skeleton. IMPLEMENT IT.


(define do_measure (lambda ()
  (run 2 (x)
    (fresh (p q r)
      (== x x)))
))

(if (not (getenv "DONT_RUN_CHEZ"))
  (begin
    (list-display (do_measure))
    (report_counters)
    (exit)
      ))
