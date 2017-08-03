In the beginning please do

```
  # ocaml 4.04.0
  git clone https://github.com/Kakadu/ocanren-perf.git -b makeItFaster  
  git submodule update --init
  opam update # if necessary
  opam pin add ppx_deriving https://github.com/whitequark/ppx_deriving.git
  opam install ppx_tools_versioned ppx_deriving mtime
  opam pin add GT https://github.com/Kakadu/GT.git\#for_bench
  # install Chez scheme somehow from https://github.com/cisco/ChezScheme
  make # to build graph
```

and install `gnuplot` using your package manager.


## Tips and tricks

### How to get profile information

```
  export OCAMLRUNPARAM='s=250M,h=250M' && perf record -g --call-graph=fp ./test001_expo1.native
  perf report #--no-children
```
