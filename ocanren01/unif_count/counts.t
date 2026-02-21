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
