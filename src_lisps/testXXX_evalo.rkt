#lang racket/base
(require racket/include)

(require "../faster-miniKanren/mk.rkt")
;(include "../faster-miniKanren/mk.scm")

(require "q.rkt")
;(include "list-display.scm")

; quines
;(list-display
  (myrun1 1 (q)
    (fresh (temp q1)
      (== q1
              '(seq ((seq ((symb 'lambda)
                          (seq ((symb temp)))
                          (seq ((symb 'list)
                                ;(symb temp)
                                ;(seq  ( ('symb 'list)
                                ;        (seq ( (symb 'quote)
                                ;               (symb 'quote)))
                                ;        ;('symb temp)
                                ;      )
                                ;)
                                ))))
                    (seq ((symb 'quote)
                          (seq ((symb 'lambda)
                                ;(seq ((symb temp)))
                                ;(seq ((symb 'list)
                                ;      (symb temp)
                                ;      (seq ((symb 'list)
                                ;            (seq  ( (symb 'quote)
                                ;                    (symb 'quote)
                                ;                  ))
                                ;            (symb temp)))))
                                            ))))
                    )) )
      (eval-expo q1 '() `(val_ ,q))
    )
)
;)

(report_counters)
