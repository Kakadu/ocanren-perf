  $ ./run_scheme.exe -quines -n 1
  (seq ((seq ((symb 'lambda) (seq ((symb '_.2032 =/= [ list
  quote ]) )) (seq ((symb 'list) (symb '_.2032 =/= [ list
  quote ]) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (symb '_.2032 =/= [ list
  quote ]) )) )) )) (seq ((symb 'quote) (seq ((symb 'lambda) (seq ((symb '_.2032 =/= [ list
  quote ]) )) (seq ((symb 'list) (symb '_.2032 =/= [ list
  quote ]) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (symb '_.2032 =/= [ list
  quote ]) )) )) )) )) ))

  unifications: 2085

  $ ./run_scheme.exe -quines -n 2
  unifications: 6920

  $ ./run_scheme.exe -quines -n 8 -q
  unifications: 14491

  $ ./run_scheme.exe -quines -n 9 -q
  unifications: 18686

  $ ./run_scheme.exe -quines -n 10 -q
  unifications: 18797

  $ ./run_scheme.exe -twines -n 1
  (seq ((symb 'quote) (seq ((seq ((symb 'lambda) (seq ((symb '_.16072 =/= [ list
  quote ]) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (symb '_.16072 =/= [ list
  quote ]) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (symb '_.16072 =/= [ list
  quote ]) )) )) )) )) (seq ((symb 'quote) (seq ((symb 'lambda) (seq ((symb '_.16072 =/= [ list
  quote ]) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (symb '_.16072 =/= [ list
  quote ]) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (symb '_.16072 =/= [ list
  quote ]) )) )) )) )) )) )) )),
  (seq ((seq ((symb 'lambda) (seq ((symb '_.16072 =/= [ list
  quote ]) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (symb '_.16072 =/= [ list
  quote ]) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (symb '_.16072 =/= [ list
  quote ]) )) )) )) )) (seq ((symb 'quote) (seq ((symb 'lambda) (seq ((symb '_.16072 =/= [ list
  quote ]) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (symb '_.16072 =/= [ list
  quote ]) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (symb '_.16072 =/= [ list
  quote ]) )) )) )) )) )) ))

  unifications: 16583

  $ ./run_scheme.exe -twines -n 2
  unifications: 55721
  $ ./run_scheme.exe -twines -n 10
  unifications: 97075

  $ ./run_scheme.exe -thrines -n 1
  * (seq ((symb 'quote) (seq ((symb 'quote) (seq ((seq ((symb 'lambda) (seq ((symb '_.63737 =/= [ list
  quote ]) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (symb '_.63737 =/= [ list
  quote ]) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (symb '_.63737 =/= [ list
  quote ]) )) )) )) )) )) (seq ((symb 'quote) (seq ((symb 'lambda) (seq ((symb '_.63737 =/= [ list
  quote ]) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (symb '_.63737 =/= [ list
  quote ]) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (symb '_.63737 =/= [ list
  quote ]) )) )) )) )) )) )) )) )) ))
    (seq ((symb 'quote) (seq ((seq ((symb 'lambda) (seq ((symb '_.63737 =/= [ list
  quote ]) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (symb '_.63737 =/= [ list
  quote ]) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (symb '_.63737 =/= [ list
  quote ]) )) )) )) )) )) (seq ((symb 'quote) (seq ((symb 'lambda) (seq ((symb '_.63737 =/= [ list
  quote ]) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (symb '_.63737 =/= [ list
  quote ]) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (symb '_.63737 =/= [ list
  quote ]) )) )) )) )) )) )) )) ))
    (seq ((seq ((symb 'lambda) (seq ((symb '_.63737 =/= [ list
  quote ]) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (symb '_.63737 =/= [ list
  quote ]) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (symb '_.63737 =/= [ list
  quote ]) )) )) )) )) )) (seq ((symb 'quote) (seq ((symb 'lambda) (seq ((symb '_.63737 =/= [ list
  quote ]) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (seq ((symb 'list) (symb '_.63737 =/= [ list
  quote ]) (seq ((symb 'list) (seq ((symb 'quote) (symb 'quote) )) (symb '_.63737 =/= [ list
  quote ]) )) )) )) )) )) )) ))


  unifications: 66826
  $ ./run_scheme.exe -thrines -n 2
  unifications: 224658


  $ ./run_scheme.exe -quines-nodiseq -n 1
  (((('lambda (vr _.2472) (('list (vr _.2472) (('list (('quote 'quote )) (vr _.2472) )) )) )) (('quote (('lambda (vr _.2472) (('list (vr _.2472) (('list (('quote 'quote )) (vr _.2472) )) )) )) )) ))

  unifications: 3490
  $ ./numero.exe --mul1x1
  multo (build_num 1) (build_num 1)
    0:	[1]
  unifications: 6
  $ ./numero.exe --mul1x2
  multo (build_num 1) (build_num 2)
    0:	[0; 1]
  unifications: 6
  $ ./numero.exe --mul2x2
  2x2=?
    0:	[0; 0; 1]
  unifications: 19
  $ ./numero.exe --mul2x3
  multo (build_num 2) (build_num 3)
    0:	[0; 1; 1]
  unifications: 19
  $ ./numero.exe --mul3x2
  3x2=?
    0:	[0; 1; 1]
  unifications: 33
  $ ./numero.exe --mul3x3
  multo (build_num 3) (build_num 3)
    0:	[1; 0; 0; 1]
  unifications: 219
  $ ./numero.exe --mul7x7
  multo (build_num 7) (build_num 7)
    0:	[1; 0; 0; 0; 1; 1]
  unifications: 1196
  $ ./numero.exe --exp2x3
  expo (build_num 2) (build_num 3)
    0:	[0; 0; 0; 1]
  unifications: 128
  $ ./numero.exe --exp3x5
  expo (build_num 3) (build_num 5)
    0:	[1; 1; 0; 0; 1; 1; 1; 1]
  unifications: 433854
  $ ./numero.exe --exp7x2
  expo (build_num 7) (build_num 2)
    0:	[1; 0; 0; 0; 1; 1]
  unifications: 368311
  $ ./numero.exe --logo1base1
  fun q -> logo (build_num 1) (build_num 1) q (build_num 0)
    0:	[]
  unifications: 5
  $ ./numero.exe --logo2base2
  fun q -> logo (build_num 2) (build_num 2) q (build_num 0)
    0:	[1]
  unifications: 52
  $ ./numero.exe --logo3base2
  fun q -> logo (build_num 3) (build_num 2) q (build_num 0)
  unifications: 168
  $ ./numero.exe --logo4base2
  fun q -> logo (build_num 4) (build_num 2) q (build_num 0)
    0:	[0; 1]
  unifications: 164
  $ ./numero.exe --logo3base3
  fun q -> logo (build_num 3) (build_num 3) q (build_num 0)
    0:	[1]
  unifications: 75
  $ ./numero.exe --logo4base3
  fun q -> logo (build_num 4) (build_num 3) q (build_num 0)
  unifications: 7777
  $ ./numero.exe --logo5base2
  fun q -> logo (build_num 5) (build_num 2) q (build_num 0)
  unifications: 600
  $ ./numero.exe --logo8base2
  fun q -> logo (build_num 8) (build_num 2) q (build_num 0)
    0:	[1; 1]
  unifications: 204
  $ ./numero.exe --logo243base3
  fun q -> logo (build_num 243) (build_num 3) q (build_num 0)
    0:	[1; 0; 1]
  unifications: 56264
  $ ./numero.exe --appendo1234
  fun q -> appendo (inj2 1 2) (inj2 3 4) q
    0:	[1; 2; 3; 4]
  unifications: 8
  $ ./numero.exe --reverso123
  fun q -> reverso (inj3 1 2 3) q
    0:	[3; 2; 1]
  unifications: 71
