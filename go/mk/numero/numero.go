// Package numero ports OCanren's relational binary arithmetic from
// ocanren-bench/ocanren01/numero_decls.ml. Numbers are lists of bits (KInt 0/1),
// most-significant bit first; the empty list is zero. Every relation is written
// with the same conde-branch order and fresh-variable order as the OCaml, so the
// search trace -- and hence the unification count -- matches OCanren exactly.
package numero

import "ocanren-go/mk"

// Cached injected constants (deterministic, like OCanren's inj / !< / %<).
var (
	bit0  = mk.MkInt(0)
	bit1  = mk.MkInt(1)
	one   = mk.MkCons(bit1, mk.MkNil()) // [1]
	three = mk.MkCons(bit1, mk.MkCons(bit1, mk.MkNil())) // [1; 1] = 3
)

func Nil() mk.Term { return mk.MkNil() }

func Cons(h, t mk.Term) mk.Term { return mk.MkCons(h, t) }

// cons2 is OCanren's `%<`: the two-element list [a; b].
func cons2(a, b mk.Term) mk.Term { return mk.MkCons(a, mk.MkCons(b, mk.MkNil())) }

// BuildNum builds the binary representation of n, like build_num. OCanren's
// build_num is LSB-first: `build_num n = [n%2; build_num (n/2)]`, so 2=[0;1] and
// 4=[0;0;1] (least-significant bit at the head of the list).
func BuildNum(n int64) mk.Term {
	var bits []mk.Term
	for n > 0 {
		bits = append(bits, mk.MkInt(n%2)) // LSB first
		n /= 2
	}
	acc := mk.MkNil()
	for i := len(bits) - 1; i >= 0; i-- {
		acc = mk.MkCons(bits[i], acc)
	}
	return acc
}

// conjAll builds a left-associative conjunction, executed left to right.
func conjAll(gs ...mk.Goal) mk.Goal {
	acc := gs[0]
	for i := 1; i < len(gs); i++ {
		acc = mk.Conj(acc, gs[i])
	}
	return acc
}

// lazy defers construction of the goal until it is applied to a state. This
// mirrors OCanren, where every goal is a function of the state: writing
// `addero d n m r` (without st) yields a cheap closure, and the conde body --
// including any recursive `addero ...` calls in its branches -- is only built
// when the goal is actually run against a state. Without this, a relation whose
// conde branches call itself directly (Addero) would recurse forever at goal-
// construction time.
func lazy(f func() mk.Goal) mk.Goal {
	return func(st mk.State) mk.Stream[mk.State] { return f()(st) }
}

// Appendo l s out.
func Appendo(l, s, out mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Conj(mk.UnifyGoal(l, Nil()), mk.UnifyGoal(s, out)),
		mk.Fresh3(func(a, d, res mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(Cons(a, d), l),
				mk.UnifyGoal(Cons(a, res), out),
				Appendo(d, s, res))
		}),
	})
}

// Zeroo n: n is zero.
func Zeroo(n mk.Term) mk.Goal { return mk.UnifyGoal(n, Nil()) }

// Poso n: n is a non-empty list.
func Poso(n mk.Term) mk.Goal {
	return mk.Fresh2(func(h, t mk.Term) mk.Goal {
		return mk.UnifyGoal(n, Cons(h, t))
	})
}

// Gt1o n: n has at least two elements.
func Gt1o(n mk.Term) mk.Goal {
	return mk.Fresh3(func(a, ad, dd mk.Term) mk.Goal {
		return mk.UnifyGoal(n, Cons(a, Cons(ad, dd)))
	})
}

// FullAddero b x y r c: satisfies b + x + y = r + 2*c.
func FullAddero(b, x, y, r, c mk.Term) mk.Goal {
	bit := func(v int64) mk.Term {
		if v == 0 {
			return bit0
		}
		return bit1
	}
	return mk.Conde([]mk.Goal{
		conjAll(mk.UnifyGoal(bit(0), b), mk.UnifyGoal(bit(0), x), mk.UnifyGoal(bit(0), y), mk.UnifyGoal(bit(0), r), mk.UnifyGoal(bit(0), c)),
		conjAll(mk.UnifyGoal(bit(1), b), mk.UnifyGoal(bit(0), x), mk.UnifyGoal(bit(0), y), mk.UnifyGoal(bit(1), r), mk.UnifyGoal(bit(0), c)),
		conjAll(mk.UnifyGoal(bit(0), b), mk.UnifyGoal(bit(1), x), mk.UnifyGoal(bit(0), y), mk.UnifyGoal(bit(1), r), mk.UnifyGoal(bit(0), c)),
		conjAll(mk.UnifyGoal(bit(1), b), mk.UnifyGoal(bit(1), x), mk.UnifyGoal(bit(0), y), mk.UnifyGoal(bit(0), r), mk.UnifyGoal(bit(1), c)),
		conjAll(mk.UnifyGoal(bit(0), b), mk.UnifyGoal(bit(0), x), mk.UnifyGoal(bit(1), y), mk.UnifyGoal(bit(1), r), mk.UnifyGoal(bit(0), c)),
		conjAll(mk.UnifyGoal(bit(1), b), mk.UnifyGoal(bit(0), x), mk.UnifyGoal(bit(1), y), mk.UnifyGoal(bit(0), r), mk.UnifyGoal(bit(1), c)),
		conjAll(mk.UnifyGoal(bit(0), b), mk.UnifyGoal(bit(1), x), mk.UnifyGoal(bit(1), y), mk.UnifyGoal(bit(0), r), mk.UnifyGoal(bit(1), c)),
		conjAll(mk.UnifyGoal(bit(1), b), mk.UnifyGoal(bit(1), x), mk.UnifyGoal(bit(1), y), mk.UnifyGoal(bit(1), r), mk.UnifyGoal(bit(1), c)),
	})
}

// Addero d n m r: adds carry-in bit d to numbers n and m, producing r.
func Addero(d, n, m, r mk.Term) mk.Goal {
	return lazy(func() mk.Goal {
		return mk.Conde([]mk.Goal{
		conjAll(mk.UnifyGoal(bit0, d), mk.UnifyGoal(m, Nil()), mk.UnifyGoal(n, r)),
		conjAll(mk.UnifyGoal(bit0, d), mk.UnifyGoal(n, Nil()), mk.UnifyGoal(m, r), Poso(m)),
		conjAll(mk.UnifyGoal(bit1, d), mk.UnifyGoal(m, Nil()), Addero(bit0, n, one, r)),
		conjAll(mk.UnifyGoal(bit1, d), mk.UnifyGoal(n, Nil()), Poso(m), Addero(bit0, one, m, r)),
		conjAll(
			mk.UnifyGoal(n, one),
			mk.UnifyGoal(m, one),
			mk.Fresh2(func(a, c mk.Term) mk.Goal {
				return conjAll(
					mk.UnifyGoal(cons2(a, c), r),
					FullAddero(d, bit1, bit1, a, c))
			})),
		conjAll(mk.UnifyGoal(n, one), GenAddero(d, n, m, r)),
		conjAll(mk.UnifyGoal(m, one), Gt1o(n), Gt1o(r), Addero(d, one, n, r)),
		conjAll(Gt1o(n), GenAddero(d, n, m, r)),
		})
	})
}

// GenAddero is the recursive step of Addero for two multi-bit numbers.
func GenAddero(d, n, m, r mk.Term) mk.Goal {
	return mk.Fresh7(func(a, b, c, e, x, y, z mk.Term) mk.Goal {
		return conjAll(
			mk.UnifyGoal(Cons(a, x), n),
			mk.UnifyGoal(Cons(b, y), m),
			Poso(y),
			mk.UnifyGoal(Cons(c, z), r),
			Poso(z),
			FullAddero(d, a, b, c, e),
			Addero(e, x, y, z))
	})
}

// Pluso n m k: k = n + m.
func Pluso(n, m, k mk.Term) mk.Goal { return Addero(bit0, n, m, k) }

// Minuso n m k: n = m + k.
func Minuso(n, m, k mk.Term) mk.Goal { return Pluso(m, k, n) }

// BoundMulto q p n m: a bounded multiplication helper used by OddMulto.
func BoundMulto(q, p, n, m mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Conj(mk.UnifyGoal(q, Nil()), Poso(p)),
		mk.Fresh7(func(a0, a1, a2, a3, x, y, z mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(q, Cons(a0, x)),
				mk.UnifyGoal(p, Cons(a1, y)),
				mk.Conde([]mk.Goal{
					conjAll(mk.UnifyGoal(n, Nil()), mk.UnifyGoal(m, Cons(a2, z)), BoundMulto(x, y, z, Nil())),
					conjAll(mk.UnifyGoal(n, Cons(a3, z)), BoundMulto(x, y, z, m)),
				}))
		}),
	})
}

// Multo n m p: p = n * m.
func Multo(n, m, p mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Conj(mk.UnifyGoal(n, Nil()), mk.UnifyGoal(p, Nil())),
		conjAll(Poso(n), mk.UnifyGoal(m, Nil()), mk.UnifyGoal(p, Nil())),
		conjAll(mk.UnifyGoal(n, one), Poso(m), mk.UnifyGoal(m, p)),
		conjAll(Gt1o(n), mk.UnifyGoal(m, one), mk.UnifyGoal(n, p)),
		mk.Fresh2(func(x, z mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(n, Cons(bit0, x)),
				Poso(x),
				mk.UnifyGoal(p, Cons(bit0, z)),
				Poso(z),
				Gt1o(m),
				Multo(x, m, z))
		}),
		mk.Fresh2(func(x, y mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(n, Cons(bit1, x)),
				Poso(x),
				mk.UnifyGoal(m, Cons(bit0, y)),
				Poso(y),
				Multo(m, n, p))
		}),
		mk.Fresh2(func(x, y mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(n, Cons(bit1, x)),
				Poso(x),
				mk.UnifyGoal(m, Cons(bit1, y)),
				Poso(y),
				OddMulto(x, n, m, p))
		}),
	})
}

// OddMulto x n m p: the odd-multiplication step used by Multo.
func OddMulto(x, n, m, p mk.Term) mk.Goal {
	return mk.Fresh(func(q mk.Term) mk.Goal {
		return conjAll(
			BoundMulto(q, p, n, m),
			Multo(x, m, q),
			Pluso(Cons(bit0, q), m, p))
	})
}

// Eqlo n m: n and m have the same length.
func Eqlo(n, m mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Conj(mk.UnifyGoal(n, Nil()), mk.UnifyGoal(m, Nil())),
		mk.Conj(mk.UnifyGoal(n, one), mk.UnifyGoal(m, one)),
		mk.Fresh4(func(a, x, b, y mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(Cons(a, x), n),
				Poso(x),
				mk.UnifyGoal(Cons(b, y), m),
				Poso(y),
				Eqlo(x, y))
		}),
	})
}

// Ltlo n m: n has smaller length than m.
func Ltlo(n, m mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Conj(mk.UnifyGoal(n, Nil()), Poso(m)),
		mk.Conj(mk.UnifyGoal(n, one), Gt1o(m)),
		mk.Fresh4(func(a, x, b, y mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(Cons(a, x), n),
				Poso(x),
				mk.UnifyGoal(Cons(b, y), m),
				Poso(y),
				Ltlo(x, y))
		}),
	})
}

// LeLo n m: length(n) <= length(m).
func LeLo(n, m mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{Eqlo(n, m), Ltlo(n, m)})
}

// Lto n m: n < m (numeric).
func Lto(n, m mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		Ltlo(n, m),
		mk.Conj(Eqlo(n, m), mk.Fresh(func(x mk.Term) mk.Goal {
			return conjAll(Poso(x), Pluso(n, x, m))
		})),
	})
}

// Leo n m: n <= m (numeric).
func Leo(n, m mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{mk.UnifyGoal(n, m), Lto(n, m)})
}

// Splito n r l h: splits n into a low part l and a high part h with remainder r.
func Splito(n, r, l, h mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		conjAll(mk.UnifyGoal(n, Nil()), mk.UnifyGoal(h, Nil()), mk.UnifyGoal(l, Nil())),
		mk.Fresh2(func(b, n1 mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(n, Cons(bit0, Cons(b, n1))),
				mk.UnifyGoal(r, Nil()),
				mk.UnifyGoal(h, Cons(b, n1)),
				mk.UnifyGoal(l, Nil()))
		}),
		mk.Fresh(func(n1 mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(n, Cons(bit1, n1)),
				mk.UnifyGoal(r, Nil()),
				mk.UnifyGoal(n1, h),
				mk.UnifyGoal(l, one))
		}),
		mk.Fresh4(func(b, n1, a, r1 mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(n, Cons(bit0, Cons(b, n1))),
				mk.UnifyGoal(Cons(a, r1), r),
				mk.UnifyGoal(l, Nil()),
				Splito(Cons(b, n1), r1, Nil(), h))
		}),
		mk.Fresh3(func(n1, a, r1 mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(n, Cons(bit1, n1)),
				mk.UnifyGoal(r, Cons(a, r1)),
				mk.UnifyGoal(l, one),
				Splito(n1, r1, Nil(), h))
		}),
		mk.Fresh5(func(b, n1, a, r1, l1 mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(n, Cons(b, n1)),
				mk.UnifyGoal(r, Cons(a, r1)),
				mk.UnifyGoal(l, Cons(b, l1)),
				Poso(l1),
				Splito(n1, r1, l1, h))
		}),
	})
}

// Divo n m q r: n = m * q + r, with 0 <= r < m.
func Divo(n, m, q, r mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		conjAll(mk.UnifyGoal(r, n), mk.UnifyGoal(q, Nil()), Lto(n, m)),
		conjAll(mk.UnifyGoal(q, one), Eqlo(n, m), Pluso(r, m, n), Lto(r, m)),
		// OCanren's `?&[...]` is an eager fold-left bind (no extra defer): the
		// progress-making goals (ltlo/lto/poso) run before the recursive divo.
		conjAll(
			Ltlo(m, n),
			Lto(r, m),
			Poso(q),
			mk.Fresh8(func(nh, nl, qh, ql, qlm, qlmr, rr, rh mk.Term) mk.Goal {
				splitBoth := conjAll(Splito(n, r, nl, nh), Splito(q, r, ql, qh))
				inner := mk.Conde([]mk.Goal{
					conjAll(mk.UnifyGoal(nh, Nil()), mk.UnifyGoal(qh, Nil()), Minuso(nl, r, qlm), Multo(ql, m, qlm)),
					conjAll(
						Poso(nh),
						Multo(ql, m, qlm),
						Pluso(qlm, r, qlmr),
						Minuso(qlmr, nl, rr),
						Splito(rr, r, Nil(), rh),
						Divo(nh, m, qh, rh)),
				})
				return mk.Conj(splitBoth, inner)
			})),
	})
}

// RepeatedMul n q nq: nq = n * q (by repeated addition).
func RepeatedMul(n, q, nq mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		conjAll(Poso(n), mk.UnifyGoal(q, Nil()), mk.UnifyGoal(nq, one)),
		mk.Conj(mk.UnifyGoal(q, one), mk.UnifyGoal(n, nq)),
		// OCanren's `?&[...]`: eager, so gt1o runs before the recursive call.
		conjAll(
			Gt1o(q),
			mk.Fresh2(func(q1, nq1 mk.Term) mk.Goal {
				return conjAll(
					Pluso(q1, one, q),
					RepeatedMul(n, q1, nq1),
					Multo(nq1, n, nq))
			})),
	})
}

// Exp2 n b q: a helper used by Logo.
func Exp2(n, b, q mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Conj(mk.UnifyGoal(n, one), mk.UnifyGoal(q, Nil())),
		// OCanren's `?&[...]`: eager.
		conjAll(
			Gt1o(n),
			mk.UnifyGoal(q, one),
			mk.Fresh(func(s mk.Term) mk.Goal {
				return Splito(n, b, s, one)
			})),
		mk.Fresh2(func(q1, b2 mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(q, Cons(bit0, q1)),
				Poso(q1),
				Ltlo(b, n),
				Appendo(b, Cons(bit1, b), b2),
				Exp2(n, b2, q1))
		}),
		mk.Fresh4(func(q1, nh, b2, s mk.Term) mk.Goal {
			return conjAll(
				mk.UnifyGoal(q, Cons(bit1, q1)),
				Poso(q1),
				Poso(nh),
				Splito(n, b, s, nh),
				Appendo(b, Cons(bit1, b), b2),
				Exp2(nh, b2, q1))
		}),
	})
}

// Logo n b q r: b^q = n + r (relational exponentiation / logarithm).
func Logo(n, b, q, r mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		conjAll(mk.UnifyGoal(n, one), Poso(b), mk.UnifyGoal(q, Nil()), mk.UnifyGoal(r, Nil())),
		conjAll(mk.UnifyGoal(q, Nil()), Lto(n, b), Pluso(r, one, n)),
		conjAll(mk.UnifyGoal(q, one), Gt1o(b), Eqlo(n, b), Pluso(r, b, n)),
		conjAll(mk.UnifyGoal(q, one), Poso(q), Pluso(r, one, n)),
		conjAll(mk.UnifyGoal(b, Nil()), Poso(q), mk.UnifyGoal(r, n)),
		mk.Conj(
			mk.UnifyGoal(b, cons2(bit0, bit1)), // [0; 1] = 2
			mk.Fresh3(func(a, ad, dd mk.Term) mk.Goal {
				return conjAll(
					Poso(dd),
					mk.UnifyGoal(n, Cons(a, Cons(ad, dd))),
					Exp2(n, Nil(), q),
					mk.Fresh(func(s mk.Term) mk.Goal {
						return Splito(n, dd, r, s)
					}))
			})),
		// OCanren's `?&[...]`: eager.
		conjAll(
			mk.Fresh4(func(a, ad, add, ddd mk.Term) mk.Goal {
				return mk.Conde([]mk.Goal{
					mk.UnifyGoal(b, three),
					mk.UnifyGoal(b, Cons(a, Cons(ad, Cons(add, ddd)))),
				})
			}),
				Ltlo(b, n),
				mk.Fresh7(func(bw1, bw, nw, nw1, ql1, ql, s mk.Term) mk.Goal {
					return conjAll(
						Exp2(b, Nil(), bw1),
						Pluso(bw1, one, bw),
						Ltlo(q, n),
						mk.Fresh2(func(q1, bwq1 mk.Term) mk.Goal {
							return conjAll(
								Pluso(q, one, q1),
								Multo(bw, q1, bwq1),
								Lto(nw1, bwq1))
						}),
						Exp2(n, Nil(), nw1),
						Pluso(nw1, one, nw),
						Divo(nw, bw, ql1, s),
						Pluso(ql, one, ql1),
						LeLo(ql, q),
						mk.Fresh5(func(bql, qh, s2, qdh, qd mk.Term) mk.Goal {
							return conjAll(
								RepeatedMul(b, ql, bql),
								Divo(nw, bw1, qh, s2),
								Pluso(ql, qdh, qh),
								Pluso(ql, qd, q),
								Leo(qd, qdh),
								mk.Fresh3(func(bqd, bq1, bq mk.Term) mk.Goal {
									return conjAll(
										RepeatedMul(b, qd, bqd),
										Multo(bql, bqd, bq),
										Multo(b, bq, bq1),
										Pluso(bq, r, n),
										Lto(n, bq1))
								}))
						}))
				}),
		),
	})
}

// Expo b q n: n = b^q (i.e. logo n b q zero).
func Expo(b, q, n mk.Term) mk.Goal { return Logo(n, b, q, Nil()) }
