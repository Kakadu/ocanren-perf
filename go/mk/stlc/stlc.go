// Package stlc holds the shared simply-typed lambda calculus constructors and
// ground variable names, ported from ../cpp/src/stlc.h.
//
//	GLam.t = V of name | App of m n | Abs of x l
//	GTyp.t = P of string | Arr of t1 t2
package stlc

import "ocanren-go/mk"

func VarX() mk.Term { return mk.MkStr("x") }
func VarY() mk.Term { return mk.MkStr("y") }
func VarF() mk.Term { return mk.MkStr("f") }

func V(name mk.Term) mk.Term     { return mk.MkLamV(name) }
func App(m, n mk.Term) mk.Term   { return mk.MkLamApp(m, n) }
func Abs(x, l mk.Term) mk.Term   { return mk.MkLamAbs(x, l) }
func P(s mk.Term) mk.Term        { return mk.MkTypeP(s) }
func Arr(t1, t2 mk.Term) mk.Term { return mk.MkTypeArr(t1, t2) }
func Pair(a, b mk.Term) mk.Term  { return mk.MkPair(a, b) }
