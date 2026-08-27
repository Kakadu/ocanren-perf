
  $ ./run_scheme.exe -quines -n 1 -q
  unifications: 2085
  last known var: 2783

  $ ./run_scheme.exe -quines -n 2 -q
  unifications: 6920
  last known var: 9197

  $ ./run_scheme.exe -quines -n 8 -q
  unifications: 14491
  last known var: 19147

  $ ./run_scheme.exe -quines -n 9 -q
  unifications: 18686
  last known var: 24852

  $ ./run_scheme.exe -quines -n 10 -q
  unifications: 18797
  last known var: 24991

  $ ./run_scheme.exe -twines -n 1 -q
  unifications: 16583
  last known var: 22638

  $ ./run_scheme.exe -twines -n 2 -q
  unifications: 55721
  last known var: 76349
  $ ./run_scheme.exe -twines -n 10 -q
  unifications: 97075
  last known var: 132904

  $ ./run_scheme.exe -thrines -n 1 -q
  unifications: 66826
  last known var: 90949

  $ ./run_scheme.exe -thrines -n 2 -q
  unifications: 224658
  last known var: 306686


  $ ./run_scheme.exe -quines-nodiseq -n 1 -q
  unifications: 3490
  last known var: 4314

  $ ./numero.exe --mul1x1
  multo (build_num 1) (build_num 1)
    0:	[1]
  unifications: 6
  last known var: 15
  $ ./numero.exe --mul1x2
  multo (build_num 1) (build_num 2)
    0:	[0; 1]
  unifications: 6
  last known var: 15
  $ ./numero.exe --mul2x2
  2x2=?
    0:	[0; 0; 1]
  unifications: 19
  last known var: 33
  $ ./numero.exe --mul2x3
  multo (build_num 2) (build_num 3)
    0:	[0; 1; 1]
  unifications: 19
  last known var: 33
  $ ./numero.exe --mul3x2
  3x2=?
    0:	[0; 1; 1]
  unifications: 33
  last known var: 50
  $ ./numero.exe --mul3x3
  multo (build_num 3) (build_num 3)
    0:	[1; 0; 0; 1]
  unifications: 219
  last known var: 209
  $ ./numero.exe --mul7x7
  multo (build_num 7) (build_num 7)
    0:	[1; 0; 0; 0; 1; 1]
  unifications: 1196
  last known var: 1173
  $ ./numero.exe --exp2x3
  expo (build_num 2) (build_num 3)
    0:	[0; 0; 0; 1]
  unifications: 128
  last known var: 155
  $ ./numero.exe --exp3x5
  expo (build_num 3) (build_num 5)
    0:	[1; 1; 0; 0; 1; 1; 1; 1]
  unifications: 433854
  last known var: 424760
  $ ./numero.exe --exp7x2
  expo (build_num 7) (build_num 2)
    0:	[1; 0; 0; 0; 1; 1]
  unifications: 368311
  last known var: 329719
  $ ./numero.exe --logo1base1
  fun q -> logo (build_num 1) (build_num 1) q (build_num 0)
    0:	[]
  unifications: 5
  last known var: 13
  $ ./numero.exe --logo2base2
  fun q -> logo (build_num 2) (build_num 2) q (build_num 0)
    0:	[1]
  unifications: 52
  last known var: 62
  $ ./numero.exe --logo3base2
  fun q -> logo (build_num 3) (build_num 2) q (build_num 0)
  unifications: 168
  last known var: 152
  $ ./numero.exe --logo4base2
  fun q -> logo (build_num 4) (build_num 2) q (build_num 0)
    0:	[0; 1]
  unifications: 164
  last known var: 234
  $ ./numero.exe --logo3base3
  fun q -> logo (build_num 3) (build_num 3) q (build_num 0)
    0:	[1]
  unifications: 75
  last known var: 88
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
  last known var: 283
  $ ./numero.exe --logo243base3
  fun q -> logo (build_num 243) (build_num 3) q (build_num 0)
    0:	[1; 0; 1]
  unifications: 56264
  last known var: 61459
  $ ./numero.exe --appendo1234
  fun q -> appendo (inj2 1 2) (inj2 3 4) q
    0:	[1; 2; 3; 4]
  unifications: 8
  last known var: 17
  $ ./numero.exe --reverso123
  fun q -> reverso (inj3 1 2 3) q
    0:	[3; 2; 1]
  unifications: 71
  last known var: 68
