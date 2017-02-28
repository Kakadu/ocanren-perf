In the beginning please do

```
  git submodule init
  git submodule update --remote
```

and install `gnuplot` using your package manager.

### Testsuite

Какие миниканрены мы тестировали?

  * различные варианты ocanren
  * faster-miniKanren на Chez Scheme 9.4.1 и на Racket 6.6
  * uKanren на Chez Scheme

В плане скорости лиспов микроканрен тормозит, схемовский на глазок чуть быстрее
ракетовского, но там не такоя большая разница по сравнению с нами: предлагаю
считать faster-miniKanren на схеме примерно таким же хорошим как на ракете.

Какие тестовые программы у нас есть.
Длинные, содержательные тесты, где ocanren тормозит
* `fun q -> expo (build_num 3) (build_num 5) q `
* `find_thrines 2`

Короткие тесты, куда вмешивается разогрев виртуальной машины. На них мы работаем
также, а иногда и шустрее.
* `fun q       -> logo (build_num 14) (build_num 2) (build_num 3) q`
* `fun q       -> logo (build_num 1025) (build_num 2) q (build_num 1)`
* `fun q       -> sorto (inj_nat_list [4;3;2;1]) q`
  Этот тест подозрителен, так как я на прошлой неделе узнал, что лисповая
  версия, написанная мною работает неправильно. И я так и не понял почему.
* `find_twines 2`
* `find_quines 5`

В общем и целом тесты, связанные с квайнами подозрительные, так как там
логические переменные плодятся гораздо чаще, чем в ракетовском "эквиваленте"
(на простых тестах плодятся ровно так же, так что проблема исключительно в
  квайнах).


## Tips and tricks

### How to get profile information

```
  export OCAMLRUNPARAM='s=250M,h=250M' && perf record -g --call-graph=fp ./test001_expo1.native
  perf report #--no-children
```
