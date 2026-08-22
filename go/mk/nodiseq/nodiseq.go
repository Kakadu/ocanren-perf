// Package nodiseq ports the quine-generating interpreter from
// ocanren01/Quines_NoDiseq.ml (Nada Amin's q_nodiseq.scm). Unlike the
// quines/twines/thrines search in package thr, this one uses ONLY miniKanren
// unification: disequality between peano variable indices is expressed by the
// relational `neq` (a conde over zero/succ), so there is no built-in disequality
// constraint. The produced quine is identical to OCanren's; the unification count
// runs higher than OCanren's because this port drives plain lazy streams rather
// than OCanren's fairly-interleaved goal scheduler (see AGENTS.md).
//
//	Gterm.t   = Symb of s | VR of n (peano) | Tuple of xs
//	Gresult.t = Closure of env*var*term | Code of t
package nodiseq

import "ocanren-go/mk"

// Cached ground terms (the arena is never reset; reusing a constant's index
// avoids unbounded growth during the search).
var (
	natZero   = mk.MkNatZero()
	nilEnv    = mk.MkNil()
	symQuote  = mk.MkGSymb(mk.MkStr("quote"))
	symList   = mk.MkGSymb(mk.MkStr("list"))
	symLambda = mk.MkGSymb(mk.MkStr("lambda"))
)

// --- Gterm constructors (OCanren's Gterm module) --------------------------

// vr is `vr n = VR n`: a variable indexed by a peano number.
func vr(n mk.Term) mk.Term { return mk.MkGtVR(n) }

// tuple is `tuple xs = Tuple [xs]`.
func tuple(xs ...mk.Term) mk.Term {
	l := mk.MkNil()
	for i := len(xs) - 1; i >= 0; i-- {
		l = mk.MkCons(xs[i], l)
	}
	return mk.MkGtTuple(l)
}

// app is `app func arg = Tuple [func; arg]`.
func app(f, a mk.Term) mk.Term { return tuple(f, a) }

// lambda is `lambda n body = Tuple [symb "lambda"; n; body]`.
func lambda(n, body mk.Term) mk.Term { return tuple(symLambda, n, body) }

// list2 is `list2 a b = Tuple [symb "list"; a; b]`.
func list2(a, b mk.Term) mk.Term { return tuple(symList, a, b) }

// --- Gresult constructors --------------------------------------------------

// clo is `closure e n t = Closure (e, n, t)`.
func clo(e, n, t mk.Term) mk.Term { return mk.MkGrClo3(e, n, t) }

// code is `code t = Code t`.
func code(t mk.Term) mk.Term { return mk.MkGtCode(t) }

// --- relations -------------------------------------------------------------

// nat holds of the peano numbers: zero or succ of a smaller number.
func nat(o mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.UnifyGoal(o, natZero),
		mk.Fresh(func(n mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(o, mk.MkNatSucc(n)), nat(n))
		}),
	})
}

// neq holds of two DISTINCT peano numbers. It is the relational stand-in for
// the built-in disequality: it decomposes both arguments into zero/succ so that
// the only way to satisfy it is for them to differ somewhere.
func neq(n1, n2 mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Conj(mk.UnifyGoal(n1, natZero),
			mk.Fresh(func(prev mk.Term) mk.Goal { return mk.UnifyGoal(n2, mk.MkNatSucc(prev)) })),
		mk.Conj(mk.UnifyGoal(n2, natZero),
			mk.Fresh(func(prev mk.Term) mk.Goal { return mk.UnifyGoal(n1, mk.MkNatSucc(prev)) })),
		mk.Fresh2(func(p1, p2 mk.Term) mk.Goal {
			return mk.Conj(mk.Conj(mk.UnifyGoal(n1, mk.MkNatSucc(p1)), mk.UnifyGoal(n2, mk.MkNatSucc(p2))), neq(p1, p2))
		}),
	})
}

// tm holds of well-formed terms: a variable, the quote symbol, a lambda, or a
// two-argument list. (Used to constrain values; not reached by the quine query.)
func tm(o mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Fresh(func(n mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(o, vr(n)), nat(n))
		}),
		mk.UnifyGoal(o, symQuote),
		mk.Fresh2(func(n, t mk.Term) mk.Goal {
			return mk.Conj(mk.Conj(mk.UnifyGoal(o, lambda(vr(n), t)), nat(n)), tm(t))
		}),
		mk.Fresh2(func(t1, t2 mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(o, tuple(symList, t1, t2)), mk.Conj(tm(t1), tm(t2)))
		}),
	})
}

// vl holds of values: a closure (with a valid environment and body) or code.
func vl(o mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Fresh3(func(e, n, t mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(o, clo(e, n, t)), mk.Conj(venv(e), mk.Conj(nat(n), tm(t))))
		}),
		mk.Fresh(func(t mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(o, code(t)), tm(t))
		}),
	})
}

// venv holds of environments: the empty list or a (peano, value) pair consed on.
func venv(o mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.UnifyGoal(o, nilEnv),
		mk.Fresh3(func(n, v, e mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(o, mk.MkCons(mk.MkPair(n, v), e)), mk.Conj(nat(n), mk.Conj(vl(v), venv(e))))
		}),
	})
}

// vlookup looks up the peano index x in environment env, yielding value v. The
// head key must equal x (first branch) or differ from it (neq, second branch,
// recurse on the tail). There is no base case for an empty env: a free variable
// simply has no binding.
func vlookup(env, x, v mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Fresh(func(er mk.Term) mk.Goal {
			return mk.UnifyGoal(env, mk.MkCons(mk.MkPair(x, v), er))
		}),
		mk.Fresh3(func(y, vy, er mk.Term) mk.Goal {
			return mk.Conj(mk.Conj(mk.UnifyGoal(env, mk.MkCons(mk.MkPair(y, vy), er)), neq(x, y)), vlookup(er, x, v))
		}),
	})
}

// Ev evaluates Gterm t in environment e, yielding the value v. It handles
// variable lookup, lambda (to a closure), quote (to code), application, and the
// two-argument list built-in.
func Ev(e, t, v mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Fresh(func(x mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(t, vr(x)), vlookup(e, x, v))
		}),
		mk.Fresh2(func(x, t0 mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(t, lambda(vr(x), t0)), mk.UnifyGoal(v, clo(e, x, t0)))
		}),
		mk.Fresh(func(t0 mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(t, app(symQuote, t0)), mk.UnifyGoal(v, code(t0)))
		}),
		mk.Fresh6(func(t1, t2, e0, x0, t0, v2 mk.Term) mk.Goal {
			return mk.Conj(
				mk.UnifyGoal(t, app(t1, t2)),
				mk.Conj(Ev(e, t1, clo(e0, x0, t0)),
					mk.Conj(Ev(e, t2, v2),
						Ev(mk.MkCons(mk.MkPair(x0, v2), e0), t0, v))))
		}),
		mk.Fresh4(func(t1, t2, c1, c2 mk.Term) mk.Goal {
			return mk.Conj(
				mk.UnifyGoal(t, list2(t1, t2)),
				mk.Conj(mk.UnifyGoal(v, code(app(c1, c2))),
					mk.Conj(Ev(e, t1, code(c1)),
						Ev(e, t2, code(c2)))))
		}),
	})
}

// Quineo is `quineo q = ev nil q (code q)`: q evaluates to itself (as code) in
// the empty environment.
func Quineo(q mk.Term) mk.Goal {
	return Ev(nilEnv, q, code(q))
}
