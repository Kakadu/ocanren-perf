
  $ cd src_lisps
  $ racket ../run.rkt --firstQ 1
  ((seq ((seq ((symb (quote lambda)) (seq ((symb _.0))) (seq ((symb (quote list)) (symb _.0) (seq ((symb (quote list)) (seq ((symb (quote quote)) (symb (quote quote)))) (symb _.0))))))) (seq ((symb (quote quote)) (seq ((symb (quote lambda)) (seq ((symb _.0))) (seq ((symb (quote list)) (symb _.0) (seq ((symb (quote list)) (seq ((symb (quote quote)) (symb (quote quote)))) (symb _.0))))))))))) (=/= ((_.0 list)) ((_.0 quote))))
  2085 unifications.
  $ racket ../run.rkt --firstQ 2
  ((seq ((seq ((symb (quote lambda)) (seq ((symb _.0))) (seq ((symb (quote list)) (symb _.0) (seq ((symb (quote list)) (seq ((symb (quote quote)) (symb (quote quote)))) (symb _.0))))))) (seq ((symb (quote quote)) (seq ((symb (quote lambda)) (seq ((symb _.0))) (seq ((symb (quote list)) (symb _.0) (seq ((symb (quote list)) (seq ((symb (quote quote)) (symb (quote quote)))) (symb _.0))))))))))) (=/= ((_.0 list)) ((_.0 quote))))
  ((seq ((seq ((symb (quote lambda)) (seq ((symb _.0))) (seq ((symb (quote list)) (seq ((seq ((symb (quote lambda)) (seq ((symb _.1))) (symb _.0))) (seq ((symb (quote quote)) _.2)))) (seq ((symb (quote list)) (seq ((symb (quote quote)) (symb (quote quote)))) (symb _.0))))))) (seq ((symb (quote quote)) (seq ((symb (quote lambda)) (seq ((symb _.0))) (seq ((symb (quote list)) (seq ((seq ((symb (quote lambda)) (seq ((symb _.1))) (symb _.0))) (seq ((symb (quote quote)) _.2)))) (seq ((symb (quote list)) (seq ((symb (quote quote)) (symb (quote quote)))) (symb _.0))))))))))) (=/= ((_.0 _.1)) ((_.0 lambda)) ((_.0 list)) ((_.0 quote))))
  6920 unifications.
  $ racket ../run.rkt --mul2x2
  (0 0 1)
  19 unifications.
  $ racket ../run.rkt --mul2x3
  (0 1 1)
  19 unifications.
  $ racket ../run.rkt --mul3x2
  (0 1 1)
  33 unifications.
  $ racket ../run.rkt --mul3x3
  (1 0 0 1)
  219 unifications.
  $ racket ../run.rkt --mul7x7
  (1 0 0 0 1 1)
  1196 unifications.
  $ racket ../run.rkt --exp3x5
  (1 1 0 0 1 1 1 1)
  433854 unifications.
  $ racket ../run.rkt --exp2x3
  (0 0 0 1)
  128 unifications.
  $ racket ../run.rkt --logo243base3
  (1 0 1)
  56264 unifications.
