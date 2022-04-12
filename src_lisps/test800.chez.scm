(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

; quines
(define do_measure (lambda ()
  (run 1 (q)
    (fresh (arg _3903 _asdf0 _asdf1 _asdf2)
      (==
        arg
          `(seq ( ;,_asdf2
                 (seq
                  ((symb 'lambda)
                   (seq ((symb ,_3903)))
                   (seq _asdf0
                        ;( (symb 'list)
                        ;  (symb ,_3903)
                        ;  (seq _asdf0
                              ;((symb 'list)
                              ;  (seq ((symb 'quote) (symb 'quote)))
                              ;  (symb ,_3903))
                        ;        ))
                                )))
                 (seq ((symb 'quote)
                       (seq ;,_asdf1
                            ((symb 'lambda)
                             (seq ((symb ,_3903)))
                             (seq ,_asdf0
                                  ;((symb 'list)
                                   ;(symb,_3903)
                                   ;(seq  ,_asdf0
                                        ;((symb 'list)
                                        ; (seq ((symb 'quote)
                                        ;       (symb 'quote)))
                                        ; (symb ,_3903))
                                   ;      ))
                             ))
                                )))) )
    )
      (eval-expo arg '() `(val_ ,arg)) )
)))

(if (not (getenv "BENCH_MODE"))
  (begin
    (list-display (do_measure))
    (report_counters)
    (exit)
      ))
