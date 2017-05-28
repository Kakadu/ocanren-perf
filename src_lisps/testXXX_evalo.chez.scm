(include "../faster-miniKanren/mk-vicare.scm")
(include "../faster-miniKanren/mk.scm")

(include "q.scm")

; quines
(list-display
  (run 1 (p)
    (fresh (temp)
      (== p
              '(seq ((seq ((symb 'lambda)
                          (seq ((symb temp)))
                          (seq ((symb 'list)
                                (symb temp)
                                (seq (('symb 'list) (seq ((symb 'quote) (symb 'quote))) ('symb _.0)))))))
                    (seq ((symb 'quote)
                          (seq ((symb 'lambda)
                                (seq ((symb temp)))
                                (seq ((symb 'list)
                                      (symb temp)
                                      (seq ((symb 'list)
                                            (seq ((symb 'quote)
                                                  (symb 'quote)))
                                            (symb temp))))))))))) )
      (eval-expo p '() `(val_ ,p))
    )
))

(report_counters)
