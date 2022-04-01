(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

; quines
(define do_measure (lambda ()
  (run 1 (q)
    (fresh (arg _3903)
      (==
        arg
          `(seq ((seq
                  ((symb 'lambda) (seq ((symb ,_3903))) (seq ((symb 'list) (symb ,_3903) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote))) (symb ,_3903)))))))
                 (seq ((symb 'quote)
                       (seq ((symb 'lambda)
                             (seq ((symb ,_3903)))
                             (seq ((symb 'list)
                                   (symb,_3903)
                                   (seq ((symb 'list)
                                         (seq ((symb 'quote)
                                               (symb 'quote)))
                                         (symb ,_3903)))))))))) )
    )
      (eval-expo arg '() `(val_ ,q)) )
)))

(if (not (getenv "BENCH_MODE"))
  (begin
    (list-display (do_measure))
    (report_counters)
    (exit)
      ))
