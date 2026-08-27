  $ ./run_scheme.exe -quines -n 1 -q
  unifications: 2085
  last known var: 3567


  $ ./run_scheme.exe -quines -n 2 -q
  unifications: 6920
  last known var: 11868

  $ ./run_scheme.exe -quines -n 8 -q
  unifications: 14491
  last known var: 24710

  $ ./run_scheme.exe -quines -n 9 -q
  unifications: 18686
  last known var: 31484

  $ ./run_scheme.exe -quines -n 10 -q
  unifications: 18797
  last known var: 31658

  $ ./run_scheme.exe -twines -n 1 -q
  unifications: 16583
  last known var: 28453


  $ ./run_scheme.exe -twines -n 2 -q
  unifications: 55721
  last known var: 94287
  $ ./run_scheme.exe -twines -n 10 -q
  unifications: 97075
  last known var: 163558

  $ ./run_scheme.exe -thrines -n 1 -q
  unifications: 66826
  last known var: 112845


  $ ./run_scheme.exe -thrines -n 2 -q
  unifications: 224658
  last known var: 374575


  $ ./run_scheme.exe -quines-nodiseq -n 1 -q
  unifications: 3490
  last known var: 5006

  $ ./numero.exe --mul1x1
  multo (build_num 1) (build_num 1)
    0:	[1]
  unifications: 6
  last known var: 18
  $ ./numero.exe --mul1x2
  multo (build_num 1) (build_num 2)
    0:	[0; 1]
  unifications: 6
  last known var: 18
  $ ./numero.exe --mul2x2
  2x2=?
    0:	[0; 0; 1]
  unifications: 19
  last known var: 36
  $ ./numero.exe --mul2x3
  multo (build_num 2) (build_num 3)
    0:	[0; 1; 1]
  unifications: 19
  last known var: 36
  $ ./numero.exe --mul3x2
  3x2=?
    0:	[0; 1; 1]
  unifications: 33
  last known var: 53
  $ ./numero.exe --mul3x3
  multo (build_num 3) (build_num 3)
    0:	[1; 0; 0; 1]
  unifications: 219
  last known var: 223
  $ ./numero.exe --mul7x7
  multo (build_num 7) (build_num 7)
    0:	[1; 0; 0; 0; 1; 1]
  unifications: 1196
  last known var: 1176
  $ ./numero.exe --exp2x3
  expo (build_num 2) (build_num 3)
    0:	[0; 0; 0; 1]
  unifications: 128
  last known var: 176
  $ ./numero.exe --exp3x5
  expo (build_num 3) (build_num 5)
    0:	[1; 1; 0; 0; 1; 1; 1; 1]
  unifications: 433854
  last known var: 425232
  $ ./numero.exe --exp7x2
  expo (build_num 7) (build_num 2)
    0:	[1; 0; 0; 0; 1; 1]
  unifications: 368311
  last known var: 329914
  $ ./numero.exe --logo1base1
  fun q -> logo (build_num 1) (build_num 1) q (build_num 0)
    0:	[]
  unifications: 5
  last known var: 13
  $ ./numero.exe --logo2base2
  fun q -> logo (build_num 2) (build_num 2) q (build_num 0)
    0:	[1]
  unifications: 52
  last known var: 72
  $ ./numero.exe --logo3base2
  fun q -> logo (build_num 3) (build_num 2) q (build_num 0)
  unifications: 168
  last known var: 152
  $ ./numero.exe --logo4base2
  fun q -> logo (build_num 4) (build_num 2) q (build_num 0)
    0:	[0; 1]
  unifications: 164
  last known var: 264
  $ ./numero.exe --logo3base3
  fun q -> logo (build_num 3) (build_num 3) q (build_num 0)
    0:	[1]
  unifications: 75
  last known var: 97
  $ ./numero.exe --logo4base3
  fun q -> logo (build_num 4) (build_num 3) q (build_num 0)
  unifications: 7777
  last known var: 7588
  $ ./numero.exe --logo5base2
  fun q -> logo (build_num 5) (build_num 2) q (build_num 0)
  unifications: 600
  last known var: 924
  $ ./numero.exe --logo8base2
  fun q -> logo (build_num 8) (build_num 2) q (build_num 0)
    0:	[1; 1]
  unifications: 204
  last known var: 311
  $ ./numero.exe --logo243base3
  fun q -> logo (build_num 243) (build_num 3) q (build_num 0)
    0:	[1; 0; 1]
  unifications: 56264
  last known var: 61780
  $ ./numero.exe --appendo1234
  fun q -> appendo (inj2 1 2) (inj2 3 4) q
    0:	[1; 2; 3; 4]
  unifications: 8
  last known var: 17
  $ ./numero.exe --reverso123
  fun q -> reverso (inj3 1 2 3) q
    0:	[3; 2; 1]
  unifications: 71
  last known var: 80
