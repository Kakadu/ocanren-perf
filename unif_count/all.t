
  $ cd src_lisps
  $ racket ../run.rkt -q --quines 1
  2085 unifications.
  $ racket ../run.rkt -q --quines 2
  6920 unifications.
  $ racket ../run.rkt -q --quines 10
  18798 unifications.

  $ racket ../run.rkt -q --twines 1
  16583 unifications.
  $ racket ../run.rkt -q --twines 2
  55724 unifications.
  $ racket ../run.rkt -q --twines 10
  97072 unifications.

  $ racket ../run.rkt -q --thrines 1
  66826 unifications.
  $ racket ../run.rkt -q --thrines 2
  224669 unifications.

  $ racket ../run.rkt -q --mul1x1
  6 unifications.
  $ racket ../run.rkt -q --mul1x2
  6 unifications.
  $ racket ../run.rkt -q --mul2x2
  19 unifications.
  $ racket ../run.rkt -q --mul2x3
  19 unifications.
  $ racket ../run.rkt -q --mul3x2
  33 unifications.
  $ racket ../run.rkt -q --mul3x3
  219 unifications.
  $ racket ../run.rkt -q --mul7x7
  1196 unifications.
  $ racket ../run.rkt -q --exp3x5
  433854 unifications.
  $ racket ../run.rkt -q --exp2x3
  128 unifications.
  $ racket ../run.rkt -q --exp7x2
  368311 unifications.
  $ racket ../run.rkt -q --logo2base2
  52 unifications.
  $ racket ../run.rkt -q --logo3base2
  168 unifications.
  $ racket ../run.rkt -q --logo4base2
  164 unifications.
  $ racket ../run.rkt -q --logo3base3
  75 unifications.
  $ racket ../run.rkt -q --logo4base3
  7777 unifications.
  $ racket ../run.rkt -q --logo5base2
  600 unifications.
  $ racket ../run.rkt -q --logo8base2
  204 unifications.
  $ racket ../run.rkt -q --logo243base3
  56264 unifications.

  $ racket ../run.rkt   --quines-nodiseq 1
  ((lambda (vr _.0) (list (vr _.0) (list (quote quote) (vr _.0)))) (quote (lambda (vr _.0) (list (vr _.0) (list (quote quote) (vr _.0))))))
  3490 unifications.
