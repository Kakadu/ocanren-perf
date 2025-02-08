  $ ./numero.exe --ex1
  fun q -> expo (build_num 3) (build_num 5) q
    0:	[1; 1; 0; 0; 1; 1; 1; 1]
  unifications: 403594

  $ ./run_scheme.exe -quines -n 1
  (seq [(seq [(symb 'lambda); (seq [(symb '_.2032 [=/= quote; =/= list])]); (seq [(symb 'list); (symb '_.2032 [=/= quote; =/= list]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (symb '_.2032 [=/= quote; =/= list])])])]); (seq [(symb 'quote); (seq [(symb 'lambda); (seq [(symb '_.2032 [=/= quote; =/= list])]); (seq [(symb 'list); (symb '_.2032 [=/= quote; =/= list]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (symb '_.2032 [=/= quote; =/= list])])])])])])
  
  unifications: 2085


  $ ./run_scheme.exe -twines -n 1
  (seq [(symb 'quote); (seq [(seq [(symb 'lambda); (seq [(symb '_.16072 [=/= quote; =/= list])]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (symb '_.16072 [=/= quote; =/= list]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (symb '_.16072 [=/= quote; =/= list])])])])]); (seq [(symb 'quote); (seq [(symb 'lambda); (seq [(symb '_.16072 [=/= quote; =/= list])]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (symb '_.16072 [=/= quote; =/= list]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (symb '_.16072 [=/= quote; =/= list])])])])])])])]),
  (seq [(seq [(symb 'lambda); (seq [(symb '_.16072 [=/= quote; =/= list])]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (symb '_.16072 [=/= quote; =/= list]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (symb '_.16072 [=/= quote; =/= list])])])])]); (seq [(symb 'quote); (seq [(symb 'lambda); (seq [(symb '_.16072 [=/= quote; =/= list])]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (symb '_.16072 [=/= quote; =/= list]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (symb '_.16072 [=/= quote; =/= list])])])])])])])
  
  unifications: 16583
  $ ./run_scheme.exe -thrines -n 1
  * (seq [(symb 'quote); (seq [(symb 'quote); (seq [(seq [(symb 'lambda); (seq [(symb '_.63737 [=/= quote; =/= list])]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (symb '_.63737 [=/= quote; =/= list]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (symb '_.63737 [=/= quote; =/= list])])])])])]); (seq [(symb 'quote); (seq [(symb 'lambda); (seq [(symb '_.63737 [=/= quote; =/= list])]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (symb '_.63737 [=/= quote; =/= list]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (symb '_.63737 [=/= quote; =/= list])])])])])])])])])])
    (seq [(symb 'quote); (seq [(seq [(symb 'lambda); (seq [(symb '_.63737 [=/= quote; =/= list])]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (symb '_.63737 [=/= quote; =/= list]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (symb '_.63737 [=/= quote; =/= list])])])])])]); (seq [(symb 'quote); (seq [(symb 'lambda); (seq [(symb '_.63737 [=/= quote; =/= list])]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (symb '_.63737 [=/= quote; =/= list]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (symb '_.63737 [=/= quote; =/= list])])])])])])])])])
    (seq [(seq [(symb 'lambda); (seq [(symb '_.63737 [=/= quote; =/= list])]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (symb '_.63737 [=/= quote; =/= list]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (symb '_.63737 [=/= quote; =/= list])])])])])]); (seq [(symb 'quote); (seq [(symb 'lambda); (seq [(symb '_.63737 [=/= quote; =/= list])]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (seq [(symb 'list); (symb '_.63737 [=/= quote; =/= list]); (seq [(symb 'list); (seq [(symb 'quote); (symb 'quote)]); (symb '_.63737 [=/= quote; =/= list])])])])])])])])
  
  
  unifications: 66826

