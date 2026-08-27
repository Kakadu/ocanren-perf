  $ cd src_lisps
  $ racket ../run.rkt -q --quines 1
  2085 unifications.
  last known var: 2783.

  $ racket ../run.rkt -q --quines 2
  6920 unifications.
  last known var: 9197.

  $ racket ../run.rkt -q --quines 8
  14491 unifications.
  last known var: 19147.

  $ racket ../run.rkt -q --quines 9
  18687 unifications.
  last known var: 24852.

  $ racket ../run.rkt -q --quines 10
  18798 unifications.
  last known var: 24991.

  $ racket ../run.rkt -q --twines 1
  16583 unifications.
  last known var: 22639.

  $ racket ../run.rkt -q --twines 2
  55724 unifications.
  last known var: 76355.

  $ racket ../run.rkt -q --twines 10
  97072 unifications.
  last known var: 132888.

  $ racket ../run.rkt -q --thrines 1
  66826 unifications.
  last known var: 90954.
  $ racket ../run.rkt -q --thrines 2
  224669 unifications.
  last known var: 306711.

  $ racket ../run.rkt -q --mul1x1
  6 unifications.
  last known var: 15.
  $ racket ../run.rkt -q --mul1x2
  6 unifications.
  last known var: 15.
  $ racket ../run.rkt -q --mul2x2
  19 unifications.
  last known var: 33.
  $ racket ../run.rkt -q --mul2x3
  19 unifications.
  last known var: 33.
  $ racket ../run.rkt -q --mul3x2
  33 unifications.
  last known var: 50.
  $ racket ../run.rkt -q --mul3x3
  219 unifications.
  last known var: 209.
  $ racket ../run.rkt -q --mul7x7
  1196 unifications.
  last known var: 1173.
  $ racket ../run.rkt -q --exp3x5
  433854 unifications.
  last known var: 424760.
  $ racket ../run.rkt -q --exp2x3
  128 unifications.
  last known var: 155.
  $ racket ../run.rkt -q --exp7x2
  368311 unifications.
  last known var: 329719.
  $ racket ../run.rkt -q --logo1base1
  5 unifications.
  last known var: 13.
  $ racket ../run.rkt -q --logo2base2
  52 unifications.
  last known var: 62.
  $ racket ../run.rkt -q --logo3base2
  168 unifications.
  last known var: 152.
  $ racket ../run.rkt -q --logo4base2
  164 unifications.
  last known var: 234.
  $ racket ../run.rkt -q --logo3base3
  75 unifications.
  last known var: 88.
  $ racket ../run.rkt -q --logo4base3
  7777 unifications.
  last known var: 7588.
  $ racket ../run.rkt -q --logo5base2
  600 unifications.
  last known var: 924.
  $ racket ../run.rkt -q --logo8base2
  204 unifications.
  last known var: 283.
  $ racket ../run.rkt -q --logo243base3
  56264 unifications.
  last known var: 61459.

  $ racket ../run.rkt   --quines-nodiseq 1
  ((lambda (vr _.0) (list (vr _.0) (list (quote quote) (vr _.0)))) (quote (lambda (vr _.0) (list (vr _.0) (list (quote quote) (vr _.0))))))
  3490 unifications.
  last known var: 4314.

  $ racket ../run.rkt -q --appendo1234
  8 unifications.
  last known var: 17.

  $ racket ../run.rkt -q --reverso123
  71 unifications.
  last known var: 68.
