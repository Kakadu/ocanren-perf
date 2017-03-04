In the beginning please do

```
  git submodule init
  git submodule update --remote
```

and install `gnuplot` using your package manager.

### Testsuite

Какие миниканрены мы тестировали?

  * различные варианты ocanren (компилятор 4.04.0+frame_pointer+flambda)
  * faster-miniKanren на Chez Scheme 9.4.1 и на Racket 6.6. Форк Бёрдовской
    (https://github.com/Kakadu/faster-miniKanren.git)[реализации]
  * uKanren на Chez Scheme: https://github.com/jasonhemann/microKanren.git

В плане скорости лиспов микроканрен тормозит, схемовский на глазок чуть быстрее
ракетовского, но там не такоя большая разница по сравнению с нами: предлагаю
считать faster-miniKanren на схеме примерно таким же хорошим как на ракете.

Какие тестовые программы у нас есть.

  * `fun q -> expo (build_num 3) (build_num 5) q `, три в пятой степени = 243
  * обратный предыдущему, логарифм 243 по основанию 3
  * сортировка списка из первых четырех чисел Пеанов убывающем порядке.
    `fun q       -> sorto (inj_nat_list [4;3;2;1]) q`


  * `find_thrines 3`
  * `find_twines 15`
  * `find_quines 100`

Квайны, переписанны в нашем стиле (дополнительные конструкторы вместо symbolo).
Для микроКанрена не годятся.

Запускалось в камле скомпилированном в native code виде, в схеме тоже в скомпилированном
(через `echo '(compile-file "file.scm")' | scheme -q`). На компьютере

## Tips and tricks

### How to get profile information

```
  export OCAMLRUNPARAM='s=250M,h=250M' && perf record -g --call-graph=fp ./test001_expo1.native
  perf report #--no-children
```
