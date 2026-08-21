// Package thr ports the quines/thrines search from ocanren-bench/ocanren01/
// Quine_decls.ml (and ../cpp/src/thrines.h).
//
//	Gterm.t   = Symb of s | Seq of xs        (a symbol or a sequence/list)
//	Gresult.t = Closure s t xs | Val_ of t   (a closure or a value)
//
// The relations implement an evaluator for a tiny Lisp-like language and use
// them to synthesize *thrines*: triples (p, q, r) of pairwise-different terms
// such that p evaluates to q, q to r and r back to p in the empty environment.
package thr

import (
	"fmt"

	"ocanren-go/mk"
)

// Cached ground Gterms (the term arena is never reset, so reusing the same
// immutable index for a constant avoids unbounded growth).
var (
	symQuote  = mk.MkGSymb(mk.MkStr("quote"))
	symList   = mk.MkGSymb(mk.MkStr("list"))
	symLambda = mk.MkGSymb(mk.MkStr("lambda"))
)

// List2 is a two-element logic list: [a; b].
func List2(a, b mk.Term) mk.Term { return mk.MkCons(a, mk.MkCons(b, mk.MkNil())) }

// Lookupo looks up symbol x in environment env (a list of (name, Gresult)
// pairs), yielding t. The key must match (y === x) or be different (y =/= x)
// and we recurse on the tail.
func Lookupo(x, env, t mk.Term) mk.Goal {
	return mk.Fresh3(func(rest, y, v mk.Term) mk.Goal {
		return mk.Conj(
			mk.UnifyGoal(env, mk.MkCons(mk.MkPair(y, v), rest)),
			mk.Conde([]mk.Goal{
				mk.Conj(mk.UnifyGoal(y, x), mk.UnifyGoal(v, t)),
				mk.Conj(mk.DiseqGoal(y, x), Lookupo(x, rest, t)),
			}))
	})
}

// NotInEnvo: the symbol x is not a key of environment env.
func NotInEnvo(x, env mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Fresh3(func(y, v, rest mk.Term) mk.Goal {
			return mk.Conj(
				mk.Conj(mk.UnifyGoal(env, mk.MkCons(mk.MkPair(y, v), rest)), mk.DiseqGoal(y, x)),
				NotInEnvo(x, rest))
		}),
		mk.UnifyGoal(env, mk.MkNil()),
	})
}

// ProperListo evaluates each element of the list es in env, collecting the
// resulting values into rs (element by element).
func ProperListo(es, env, rs mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Conj(mk.UnifyGoal(es, mk.MkNil()), mk.UnifyGoal(rs, mk.MkNil())),
		mk.Fresh4(func(e, d, te, td mk.Term) mk.Goal {
			return mk.Conj(
				mk.Conj(mk.Conj(mk.UnifyGoal(es, mk.MkCons(e, d)), mk.UnifyGoal(rs, mk.MkCons(te, td))),
					Evalo(e, env, mk.MkGVal(te))),
				ProperListo(d, env, td))
		}),
	})
}

// Evalo evaluates Gterm `term` in environment `env`, yielding the Gresult `r`.
// Handles quote, list, symbol lookup, application and lambda.
func Evalo(term, env, r mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		// (quote t) -> t
		mk.Fresh(func(t mk.Term) mk.Goal {
			return mk.Conj(
				mk.Conj(mk.UnifyGoal(term, mk.MkGSeq(List2(symQuote, t))),
					mk.UnifyGoal(r, mk.MkGVal(t))),
				NotInEnvo(mk.MkStr("quote"), env))
		}),
		// (list es...) -> (seq rs) where rs are the evaluated elements
		mk.Fresh2(func(es, rs mk.Term) mk.Goal {
			return mk.Conj(
				mk.Conj(mk.Conj(mk.UnifyGoal(term, mk.MkGSeq(mk.MkCons(symList, es))),
					mk.UnifyGoal(r, mk.MkGVal(mk.MkGSeq(rs)))),
					NotInEnvo(mk.MkStr("list"), env)),
				ProperListo(es, env, rs))
		}),
		// a bare symbol: look it up in the environment
		mk.Fresh(func(s mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(term, mk.MkGSymb(s)), Lookupo(s, env, r))
		}),
		// (func arg): evaluate both, then apply
		mk.Fresh6(func(funcT, arge, arg, x, body, env1 mk.Term) mk.Goal {
			return mk.Conj(
				mk.Conj(mk.Conj(mk.UnifyGoal(term, mk.MkGSeq(List2(funcT, arge))),
					Evalo(arge, env, arg)),
					Evalo(funcT, env, mk.MkGClosure(x, body, env1))),
				Evalo(body, mk.MkCons(mk.MkPair(x, arg), env1), r))
		}),
		// (lambda (x) body) -> a closure. OCanren: seq (symb "lambda" %
		// (seq !<(symb x) %< body)) -- the parameter is the 1-element list
		// [symb x], so the inner seq is [[symb x]; body].
		mk.Fresh2(func(x, body mk.Term) mk.Goal {
			param := mk.MkCons(mk.MkGSymb(x), mk.MkNil()) // [symb x]
			inner := mk.MkGSeq(mk.MkCons(param, mk.MkCons(body, mk.MkNil()))) // [[symb x]; body]
			return mk.Conj(
				mk.Conj(mk.UnifyGoal(term, mk.MkGSeq(mk.MkCons(symLambda, inner))),
					NotInEnvo(mk.MkStr("lambda"), env)),
				mk.UnifyGoal(r, mk.MkGClosure(x, body, env)))
		}),
	})
}

// Thrineso: x = (p, q, r) where p, q, r are pairwise different and p -> q -> r
// -> p under evaluation in the empty environment. The triple is encoded as a
// nested pair (p, (q, r)).
func Thrineso(x mk.Term) mk.Goal {
	return mk.Fresh3(func(p, q, r mk.Term) mk.Goal {
		return mk.Conj(
			mk.Conj(mk.Conj(mk.DiseqGoal(p, q), mk.DiseqGoal(q, r)), mk.DiseqGoal(r, p)),
			mk.Conj(
				mk.Conj(Evalo(p, mk.MkNil(), mk.MkGVal(q)), Evalo(q, mk.MkNil(), mk.MkGVal(r))),
				mk.Conj(Evalo(r, mk.MkNil(), mk.MkGVal(p)),
					mk.UnifyGoal(x, mk.MkPair(p, mk.MkPair(q, r))))))
	})
}

// Quineso is OCanren's `quineso q = evalo q nil (val_ q)`: q evaluates to
// itself in the empty environment.
func Quineso(q mk.Term) mk.Goal {
	return Evalo(q, mk.MkNil(), mk.MkGVal(q))
}

// Twineso is OCanren's `twineso q p = q =/= p &&& evalo q nil (val_ p) &&&
// evalo p nil (val_ q)`. It takes the two query terms directly (OCanren's
// `run qr twineso` creates them via call_fresh with NO extra unification), so
// there is deliberately no pair-unification here.
func Twineso(q, p mk.Term) mk.Goal {
	return mk.Conj(
		mk.DiseqGoal(q, p),
		mk.Conj(Evalo(q, mk.MkNil(), mk.MkGVal(p)),
			Evalo(p, mk.MkNil(), mk.MkGVal(q))))
}

// --- Gterm printing (OCanren's `(symb 'x)` / `(seq ...)` style) -----------

// ShowSymPayload prints a symbol payload: a ground string bare, a free variable
// as _.N.
func ShowSymPayload(t mk.Term) string {
	switch t.Kind() {
	case mk.KStr:
		return t.StrVal()
	case mk.KVar:
		return fmt.Sprintf("_.%d", t.VarVal().Index)
	default:
		return mk.Show(t)
	}
}

func ShowGterm(t mk.Term) string {
	switch t.Kind() {
	case mk.KVar:
		return fmt.Sprintf("_.%d", t.VarVal().Index)
	case mk.KGtSymb:
		return "(symb '" + ShowSymPayload(t.Car()) + ")"
	case mk.KGtSeq:
		return "(seq " + ShowGlist(t.Car()) + ")"
	default:
		return mk.Show(t) // fallback for unexpected shapes
	}
}

func ShowGlist(xs mk.Term) string {
	out := "("
	first := true
	cur := xs
	for cur.Kind() == mk.KCons {
		if !first {
			out += " "
		}
		out += ShowGterm(cur.Car())
		cur = cur.Cdr()
		first = false
	}
	if cur.Kind() != mk.KNil {
		if !first {
			out += " "
		}
		out += ShowGterm(cur)
	}
	out += ")"
	return out
}
