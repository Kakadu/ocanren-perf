In the beginning please do

```
  git submodule init
  git submodule update --remote
```

and install `gnuplot` using your package manager.


### How to get profile information

```
  export OCAMLRUNPARAM='s=250M,h=250M' && sudo perf record -g --call-graph=fp ./test001_expo1.native
  sudo perf report #--no-children
```
